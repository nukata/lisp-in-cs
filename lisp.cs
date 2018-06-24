// H29.3/1 - H30.6/24 by SUZUKI Hisao
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

// lisp.exe: csc /doc:lisp.xml /o lisp.cs
// doc: mdoc update -i lisp.xml -o xml lisp.exe; mdoc export-html -o html xml

[assembly: AssemblyProduct("Nukata Lisp Light")]
[assembly: AssemblyVersion("1.2.1.*")]
[assembly: AssemblyTitle("A Lisp interpreter in C# 7")]
[assembly: AssemblyCopyright("© 2017 Oki Software Co., Ltd.; " + 
                             "© 2018 SUZUKI Hisao [MIT License]")]

/// <summary>
///  A Lisp interpreter written in C# 7
/// </summary><remarks>
///  This is ported from Nuka Lisp in Dart
///  (https://github.com/nukata/lisp-in-dart) except for bignum.
///  Its sole numeric type is <c>double</c> in C#.
///  It is named after ex-Nukata Town in Japan.
/// </remarks>
public static class NukataLisp {

    /// <summary>Cons cell</summary>
    public sealed class Cell {
        /// <summary>Head part of the cons cell</summary>
        public object Car;
        /// <summary>Tail part of the cons cell</summary>
        public object Cdr;

        /// <summary>Construct a cons cell with its head and tail.</summary>
        public Cell(object car, object cdr) {
            Car = car;
            Cdr = cdr;
        }

        /// <summary>Make a simple string representation.</summary>
        /// <remarks>Do not invoke this for any circular list.</remarks>
        public override string ToString() =>
            $"({Car ?? "null"} . {Cdr ?? "null"})";

        /// <summary>Length as a list</summary>
        public int Length => FoldL(0, this, (i, e) => i + 1);
    }


    // MapCar((a b c), fn) => (fn(a) fn(b) fn(c))
    static Cell MapCar(Cell j, Func<object, object> fn) {
        if (j == null)
            return null;
        object a = fn(j.Car);
        object d = j.Cdr;
        if (d is Cell dc)
            d = MapCar(dc, fn);
        if (j.Car == a && j.Cdr == d)
            return j;
        return new Cell(a, d);
    }

    // FoldL(x, (a b c), fn) => fn(fn(fn(x, a), b), c)
    static T FoldL<T> (T x, Cell j, Func<T, object, T> fn) {
        while (j != null) {
            x = fn(x, j.Car);
            j = (Cell) j.Cdr;
        }
        return x;
    }


    /// <summary>Lisp symbol</summary>
    public class Sym {
        /// <summary>The symbol's name</summary>
        public string Name { get; }
        
        /// <summary>Construct a symbol that is not interned.</summary>
        public Sym(string name) {
            Name = name;
        }

        /// <summary>Return the symbol's name</summary>
        public override string ToString() => Name;
        /// <summary>Return the hashcode of the symbol's name</summary>
        public override int GetHashCode() => Name.GetHashCode();

        /// <summary>Table of interned symbols</summary>
        protected static readonly Dictionary<string, Sym> Table =
            new Dictionary<string, Sym>();

        /// <summary>Return an interned symbol for the name.</summary>
        /// <remarks>If the name is not interned yet, such a symbol
        /// will be constructed with <paramref name="make"/>.</remarks>
        protected static Sym New(string name, Func<string, Sym> make) {
            lock (Table) {
                if (! Table.TryGetValue(name, out Sym result)) {
                    result = make(name);
                    Table[name] = result;
                }
                return result;
            }
        }

        /// <summary>Construct an interned symbol.</summary>
        public static Sym New(string name) => New(name, s => new Sym(s));

        /// <summary>Is it interned?</summary>
        public bool IsInterned {
            get {
                lock (Table) {
                    return Table.TryGetValue(Name, out Sym s) &&
                        Object.ReferenceEquals(this, s);
                }
            }
        }
    }


    // Expression keyword
    sealed class Keyword: Sym {
        Keyword(string name): base(name) {}
        internal static new Sym New(string name)
            => New(name, s => new Keyword(s));
    }

    static readonly Sym CondSym = Keyword.New("cond");
    static readonly Sym LambdaSym = Keyword.New("lambda");
    static readonly Sym MacroSym = Keyword.New("macro");
    static readonly Sym PrognSym = Keyword.New("progn");
    static readonly Sym QuasiquoteSym = Keyword.New("quasiquote");
    static readonly Sym QuoteSym = Keyword.New("quote");
    static readonly Sym SetqSym = Keyword.New("setq");

    static readonly Sym BackQuoteSym = Sym.New("`");
    static readonly Sym CommaAtSym = Sym.New(",@");
    static readonly Sym CommaSym = Sym.New(",");
    static readonly Sym DotSym = Sym.New(".");
    static readonly Sym LeftParenSym = Sym.New("(");
    static readonly Sym RightParenSym = Sym.New(")");
    static readonly Sym SingleQuoteSym = Sym.New("'");

    static readonly Sym AppendSym = Sym.New("append");
    static readonly Sym ConsSym = Sym.New("cons");
    static readonly Sym ListSym = Sym.New("list");
    static readonly Sym RestSym = Sym.New("&rest");
    static readonly Sym UnquoteSym = Sym.New("unquote");
    static readonly Sym UnquoteSplicingSym = Sym.New("unquote-splicing");

    /// <summary>The symbol of <c>t</c></summary>
    public static readonly Sym TSym = Sym.New("t");


    //------------------------------------------------------------------

    // Get cdr of list x as a Cell or null.
    static Cell CdrCell(Cell x) {
        var k = x.Cdr;
        if (k == null) {
            return null;
        } else {
            if (k is Cell c)
                return c;
            else
                throw new EvalException("proper list expected", x);
        }
    }


    /// <summary>Common base class of Lisp functions</summary>
    public abstract class LispFunc {
        /// <summary>Number of arguments, made negative if the function
        /// has &amp;rest</summary>
        public int Carity { get; }

        int Arity => (Carity < 0) ? -Carity : Carity;
        bool HasRest => (Carity < 0);

        // Number of fixed arguments
        int FixedArgs => (Carity < 0) ? -Carity - 1 : Carity;

        /// <summary>Construct with Carity.</summary>
        protected LispFunc(int carity) {
            Carity = carity;
        }

        /// <summary>Make a frame for local variables from a list of
        /// actual arguments.</summary>
        public object[] MakeFrame(Cell arg) {
            var frame = new object[Arity];
            int n = FixedArgs;
            int i;
            for (i = 0; i < n && arg != null; i++) {
                // Set the list of fixed arguments.
                frame[i] = arg.Car;
                arg = CdrCell(arg);
            }
            if (i != n || (arg != null && !HasRest))
                throw new EvalException("arity not matched", this);
            if (HasRest)
                frame[n] = arg;
            return frame;
        }

        /// <summary>Evaluate each expression in a frame.</summary>
        public void EvalFrame(object[] frame, Interp interp, Cell env) {
            int n = FixedArgs;
            for (int i = 0; i < n; i++)
                frame[i] = interp.Eval(frame[i], env);
            if (HasRest) {
                if (frame[n] is Cell j) {
                    Cell z = null;
                    Cell y = null;
                    do {
                        var e = interp.Eval(j.Car, env);
                        Cell x = new Cell(e, null);
                        if (z == null)
                            z = x;
                        else
                            y.Cdr = x;
                        y = x;
                        j = CdrCell(j);
                    } while (j != null);
                    frame[n] = z;
                }
            }
        }
    }


    // Common base class of functions which are defined with Lisp expressions
    abstract class DefinedFunc: LispFunc {
        // Lisp list as the function body
        public readonly Cell Body;

        protected DefinedFunc(int carity, Cell body): base(carity) {
            Body = body;
        }
    }


    // Common function type which represents any factory method of DefinedFunc
    delegate DefinedFunc FuncFactory(int carity, Cell body, Cell env);


    // Compiled macro expression
    sealed class Macro: DefinedFunc {
        Macro(int carity, Cell body): base(carity, body) {}
        public override string ToString() => $"#<macro:{Carity}:{Str(Body)}>";

        // Expand the macro with a list of actual arguments.
        public object ExpandWith(Interp interp, Cell arg) {
            object[] frame = MakeFrame(arg);
            Cell env = new Cell(frame, null);
            object x = null;
            for (Cell j = Body; j != null; j = CdrCell(j))
                x = interp.Eval(j.Car, env);
            return x;
        }

        public static DefinedFunc Make(int carity, Cell body, Cell env) {
            Debug.Assert(env == null);
            return new Macro(carity, body);
        }
    }


    // Compiled lambda expression (Within another function)
    sealed class Lambda: DefinedFunc {
        Lambda(int carity, Cell body): base(carity, body) {}
        public override string ToString() => $"#<lambda:{Carity}:{Str(Body)}>";

        public static DefinedFunc Make(int carity, Cell body, Cell env) {
            Debug.Assert(env == null);
            return new Lambda(carity, body);
        }
    }


    // Compiled lambda expression (Closure with environment)
    sealed class Closure: DefinedFunc {
        // The environment of the closure
        public readonly Cell Env;

        Closure(int carity, Cell body, Cell env): base(carity, body) {
            Env = env;
        }

        public Closure(Lambda x, Cell env): this(x.Carity, x.Body, env) {}

        public override string ToString() =>
            $"#<closure:{Carity}:{Str(Env)}:{Str(Body)}>";

        // Make an environment to evaluate the body from a list of actual args.
        public Cell MakeEnv(Interp interp, Cell arg, Cell interpEnv) {
            object[] frame = MakeFrame(arg);
            EvalFrame(frame, interp, interpEnv);
            return new Cell(frame, Env); // Prepend the frame to this Env.
        }

        public static DefinedFunc Make(int carity, Cell body, Cell env) =>
            new Closure(carity, body, env);
    }


    /// <summary>Function type which represents any built-in function body
    /// </summary>
    public delegate object BuiltInFuncBody(object[] frame);

    /// <summary>Built-in function</summary>
    public sealed class BuiltInFunc: LispFunc {
        /// <summary>Name of this function</summary>
        public string Name { get; }
        /// <summary>C# function as the body of this function</summary>
        public BuiltInFuncBody Body { get; }

        /// <summary>Construct with Name, Carity and Body.</summary>
        public BuiltInFunc(string name, int carity, BuiltInFuncBody body)
            : base(carity) {
            Name = name;
            Body = body;
        }

        /// <summary>Return a string representation in Lisp.</summary>
        public override string ToString() => $"#<{Name}:{Carity}>";

        /// <summary>Invoke the built-in function with a list of
        /// actual arguments.</summary>
        public object EvalWith(Interp interp, Cell arg, Cell interpEnv) {
            object[] frame = MakeFrame(arg);
            EvalFrame(frame, interp, interpEnv);
            try {
                return Body(frame);
            } catch (EvalException) {
                throw;
            } catch (Exception ex) {
                throw new EvalException($"{ex} -- {Name}", frame);
            }
        }
    }


    // Bound variable in a compiled lambda/macro expression
    sealed class Arg {
        public readonly int Level;
        public readonly int Offset;
        public readonly Sym Symbol;

        public Arg(int level, int offset, Sym symbol) {
            Level = level;
            Offset = offset;
            Symbol = symbol;
        }
        
        public override string ToString() => $"#{Level}:{Offset}:{Symbol}";

        // Set a value x to the location corresponding to the variable in env.
        public void SetValue(object x, Cell env) {
            for (int i = 0; i < Level; i++)
                env = (Cell) env.Cdr;
            object[] frame = (object[]) env.Car;
            frame[Offset] = x;
        }

        // Get a value from the location corresponding to the variable in env.
        public object GetValue(Cell env) {
            for (int i = 0; i < Level; i++)
                env = (Cell) env.Cdr;
            object[] frame = (object[]) env.Car;
            return frame[Offset];
        }
    }


    /// <summary>Exception in evaluation</summary>
    public class EvalException: Exception {
        /// <summary>Stack trace of Lisp evaluation</summary>
        public List<string> Trace { get; } = new List<string>();

        /// <summary>Construct with a base message, cause, and
        /// a flag whether to quote strings in the cause.</summary>
        public EvalException(string msg, object x, bool quoteString=true)
            : base(msg + ": " + Str(x, quoteString)) {}

        /// <summary>Return a string representation which contains
        /// the message and the stack trace.</summary>
        public override string ToString() {
            var sb = new StringBuilder($"EvalException: {Message}", 0);
            foreach (string line in Trace)
                sb.Append($"\n\t{line}");
            return sb.ToString();
        }
    }


    // Exception which indicates on absense of a variable
    sealed class NotVariableException: EvalException {
        public NotVariableException(object x): base("variable expected", x) {}
    }


    //------------------------------------------------------------------

    /// <summary>Core of the Lisp interpreter</summary>
    public class Interp {
        /// <summary>Table of the global values of symbols</summary>
        protected readonly Dictionary<Sym, object> Globals =
            new Dictionary<Sym, object>();

        /// <summary>Standard out</summary>
        public TextWriter COut { get; set; } = Console.Out;

        /// <summary>Set each built-in function/variable as the global value
        /// of symbol.</summary>
        public Interp() {
            Globals[TSym] = TSym;
            Def("car", 1, a => (a[0] as Cell)?.Car);
            Def("cdr", 1, a => (a[0] as Cell)?.Cdr);
            Def("cons", 2, a => new Cell(a[0], a[1]));
            Def("atom", 1, a => (a[0] is Cell) ? null : TSym);
            Def("eq", 2, a => (a[0] == a[1]) ? TSym : null);

            Def("list", -1, a => a[0]);
            Def("rplaca", 2, a => { ((Cell) a[0]).Car = a[1]; return a[1]; });
            Def("rplacd", 2, a => { ((Cell) a[0]).Cdr = a[1]; return a[1]; });
            Def("length", 1, a => {
                    dynamic x = a[0];
                    if (x == null)
                        return 0.0;
                    return (double) x.Length;
                });
            Def("stringp", 1, a => (a[0] is string) ? TSym : null);
            Def("numberp", 1, a => (a[0] is double) ? TSym : null);
            Def("eql", 2, a => ((a[0] == null) ? ((a[1] == null) ?
                                                  TSym : null) :
                                a[0].Equals(a[1]) ? TSym : null));
            Def("<", 2, a => ((double) a[0] < (double) a[1]) ? TSym : null);
            Def("%", 2, a => (double) a[0] % (double) a[1]);
            Def("mod", 2, a => {
                    var x = (double) a[0];
                    var y = (double) a[1];
                    if ((x < 0 && y > 0) || (x > 0 && y < 0))
                        return x % y + y;
                    return x % y;
                });

            Def("+", -1, a => FoldL(0.0, (Cell) a[0],
                                    (i, j) => i + (double) j));
            Def("*", -1, a => FoldL(1.0, (Cell) a[0],
                                    (i, j) => i * (double) j));
            Def("-", -2, a => {
                    var x = (double) a[0];
                    var y = (Cell) a[1];
                    if (y == null)
                        return -x;
                    return FoldL(x, y, (i, j) => i - (double) j);
                });
            Def("/", -3, a => FoldL((double) a[0] / (double) a[1],
                                    (Cell) a[2],
                                    (i, j) => i / (double) j));
            Def("truncate", -2, a => {
                    var x = (double) a[0];
                    var y = (Cell) a[1];
                    if (y == null)
                        return Math.Truncate(x);
                    else if (y.Cdr == null)
                        return Math.Truncate(x / (double) y.Car);
                    else
                        throw new ArgumentException
                            ("one or two arguments expected");
                });

            Def("prin1", 1, a => {
                    COut.Write(Str(a[0], true)); return a[0];
                });
            Def("princ", 1, a => {
                    COut.Write(Str(a[0], false)); return a[0];
                });
            Def("terpri", 0, a => {
                    COut.WriteLine(); return TSym;
                });

            var gensymCounterSym = Sym.New("*gensym-counter*");
            Globals[gensymCounterSym] = 1.0;
            Def("gensym", 0, a => {
                    double x = (double) Globals[gensymCounterSym];
                    Globals[gensymCounterSym] = x + 1.0;
                    return new Sym($"G{(int) x}");
                });

            Def("make-symbol", 1, a => new Sym((string) a[0]));
            Def("intern", 1, a => Sym.New((string) a[0]));
            Def("symbol-name", 1, a => ((Sym) a[0]).Name);

            Def("apply", 2, a =>
                Eval(new Cell(a[0], MapCar((Cell) a[1], QqQuote)), null));

            Def("exit", 1, a => {
                    Environment.Exit((int) ((double) a[0]));
                    return null;
                });
            Def("dump", 0, a =>
                Globals.Keys.Aggregate((Cell) null, (x, y) => new Cell(y, x)));

            var assembly = Assembly.GetExecutingAssembly();
            var product = (AssemblyProductAttribute)
                Attribute.GetCustomAttribute
                (assembly, typeof(AssemblyProductAttribute));
            var version = assembly.GetName().Version;
            double iversion = version.Major + 0.1 * version.Minor +
                0.01 * version.Build;
            Globals[Sym.New("*version*")] =
                new Cell(iversion,
                         new Cell("C# 7", new Cell(product.Product, null)));
        }

        /// <summary>Define a built-in function by a name, an arity,
        /// and a body.</summary>
        public void Def(string name, int carity, BuiltInFuncBody body) {
            Globals[Sym.New(name)] = new BuiltInFunc(name, carity, body);
        }

        /// <summary>Evaluate a Lisp expression in an environment.</summary>
        public object Eval(object x, Cell env) {
            try {
                for (;;) {
                    switch (x) {
                    case Arg xarg:
                        return xarg.GetValue(env);
                    case Sym xsym:
                        try {
                            return Globals[xsym];
                        } catch (KeyNotFoundException) {
                            throw new EvalException("void variable", x);
                        }
                    case Cell xcell:
                        var fn = xcell.Car;
                        Cell arg = CdrCell(xcell);
                        if (fn is Keyword) {
                            if (fn == QuoteSym) {
                                if (arg != null && arg.Cdr == null)
                                    return arg.Car;
                                throw new EvalException("bad quote", x);
                            } else if (fn == PrognSym) {
                                x = EvalProgN(arg, env);
                            } else if (fn == CondSym) {
                                x = EvalCond(arg, env);
                            } else if (fn == SetqSym) {
                                return EvalSetQ(arg, env);
                            } else if (fn == LambdaSym) {
                                return Compile(arg, env, Closure.Make);
                            } else if (fn == MacroSym) {
                                if (env != null)
                                    throw new EvalException("nested macro", x);
                                return Compile(arg, null, Macro.Make);
                            } else if (fn == QuasiquoteSym) {
                                if (arg != null && arg.Cdr == null)
                                    x = QqExpand(arg.Car);
                                else
                                    throw new EvalException ("bad quasiquote",
                                                             x);
                            } else {
                                throw new EvalException("bad keyword", fn);
                            }
                        } else { // Application of a function
                            if (fn is Sym fnsym) {
                                // Expand fn = Eval(fn, env) here for speed.
                                try {
                                    fn = Globals[fnsym];
                                } catch (KeyNotFoundException) {
                                    throw new EvalException("undefined",
                                                            fnsym);
                                }
                            } else {
                                fn = Eval(fn, env);
                            }
                            switch (fn) {
                            case Closure fnclosure:
                                env = fnclosure.MakeEnv(this, arg, env);
                                x = EvalProgN(fnclosure.Body, env);
                                break;
                            case Macro fnmacro:
                                x = fnmacro.ExpandWith(this, arg);
                                break;
                            case BuiltInFunc fnbulitin:
                                return fnbulitin.EvalWith(this, arg, env);
                            default:
                                throw new EvalException("not appliable", fn);
                            }
                        }
                        break;
                    case Lambda xlambda:
                        return new Closure(xlambda, env);
                    default:
                        return x; // numbers, strings, null etc.
                    }
                }
            } catch (EvalException ex) {
                if (ex.Trace.Count < 10)
                    ex.Trace.Add(Str(x));
                throw ex;
            }
        }

        // (progn E1 ... En) => Evaluate E1, ... except for En and return it.
        object EvalProgN(Cell j, Cell env) {
            if (j == null)
                return null;
            for (;;) {
                var x = j.Car;
                j = CdrCell(j);
                if (j == null)
                    return x; // The tail expression to be evaluated later
                Eval(x, env);
            }
        }

        // Evaluate a conditional expression and return the selection.
        object EvalCond(Cell j, Cell env) {
            for (; j != null; j = CdrCell(j)) {
                var clause = j.Car;
                if (clause != null) {
                    if (clause is Cell k) {
                        var result = Eval(k.Car, env);
                        if (result != null) { // If the condition holds
                            Cell body = CdrCell(k);
                            if (body == null)
                                return QqQuote(result);
                            else
                                return EvalProgN(body, env);
                        }
                    } else {
                        throw new EvalException("cond test expected", clause);
                    }
                }
            }
            return null;        // No clause holds.
        }

        // (setq V1 E1 ..) => Evaluate Ei and assign it to Vi; return the last.
        object EvalSetQ(Cell j, Cell env) {
            object result = null;
            for (; j != null; j = CdrCell(j)) {
                var lval = j.Car;
                if (lval == TSym)
                    throw new EvalException("not assignable", lval);
                j = CdrCell(j);
                if (j == null)
                    throw new EvalException("right value expected", lval);
                result = Eval(j.Car, env);
                switch (lval) {
                case Arg arg:
                    arg.SetValue(result, env);
                    break;
                case Sym sym when !(sym is Keyword):
                    Globals[sym] = result;
                    break;
                default:
                    throw new NotVariableException(lval);
                }
            }
            return result;
        }

        // Compile a Lisp list (macro ...) or (lambda ...).
        DefinedFunc Compile(Cell arg, Cell env, FuncFactory make) {
            if (arg == null)
                throw new EvalException("arglist and body expected", arg);
            var table = new Dictionary<Sym, Arg>();
            bool hasRest = MakeArgTable(arg.Car, table);
            int arity = table.Count;
            Cell body = CdrCell(arg);
            body = ScanForArgs(body, table) as Cell;
            body = ExpandMacros(body, 20) as Cell; // Expand up to 20 nestings.
            body = CompileInners(body) as Cell;
            return make(hasRest ? -arity : arity, body, env);
        }

        // Expand macros and quasi-quotations in an expression.
        object ExpandMacros(object j, int count) {
            if ((j is Cell cell) && count > 0) {
                var k = cell.Car;
                if (k == QuoteSym || k == LambdaSym || k == MacroSym) {
                    return cell;
                } else if (k == QuasiquoteSym) {
                    Cell d = CdrCell(cell);
                    if (d != null && d.Cdr == null) {
                        var z = QqExpand(d.Car);
                        return ExpandMacros(z, count);
                    }
                    throw new EvalException("bad quasiquote", cell);
                } else {
                    if (k is Sym sym)
                        k = Globals.ContainsKey(sym) ? Globals[sym] : null;
                    if (k is Macro macro) {
                        Cell d = CdrCell(cell);
                        var z = macro.ExpandWith(this, d);
                        return ExpandMacros(z, count - 1);
                    } else {
                        return MapCar(cell, x => ExpandMacros(x, count));
                    }
                }
            } else {
                return j;
            }
        }

        // Replace inner lambda-expressions with Lambda instances.
        object CompileInners(object j) {
            if (j is Cell cell) {
                var k = cell.Car;
                if (k == QuoteSym) {
                    return cell;
                } else if (k == LambdaSym) {
                    Cell d = CdrCell(cell);
                    return Compile(d, null, Lambda.Make);
                } else if (k == MacroSym) {
                    throw new EvalException("nested macro", cell);
                } else {
                    return MapCar(cell, x => CompileInners(x));
                }
            } else {
                return j;
            }
        }
    }


    //------------------------------------------------------------------

    // Make an argument-table; return true if there is a rest argument.
    static bool MakeArgTable(object arg, IDictionary<Sym, Arg> table) {
        if (arg == null) {
            return false;
        } else if (arg is Cell argcell) {
            int offset = 0;     // offset value within the call-frame
            bool hasRest = false;
            for (; argcell != null; argcell = CdrCell(argcell)) {
                var j = argcell.Car;
                if (hasRest)
                    throw new EvalException("2nd rest", j);
                if (j == RestSym) { // &rest var
                    argcell = CdrCell(argcell);
                    if (argcell == null)
                        throw new NotVariableException(argcell);
                    j = argcell.Car;
                    if (j == RestSym)
                        throw new NotVariableException(j);
                    hasRest = true;
                }
                Sym sym = j as Sym;
                if (sym == null) {
                    Arg jarg = j as Arg;
                    if (jarg != null)
                        sym = jarg.Symbol;
                    else
                        throw new NotVariableException(j);
                }
                if (sym == TSym)
                    throw new EvalException("not assignable", sym);
                if (table.ContainsKey(sym))
                    throw new EvalException("duplicated argument name", sym);
                table[sym] = new Arg(0, offset, sym);
                offset++;
            }
            return hasRest;
        } else {
            throw new EvalException("arglist expected", arg);
        }
    }

    // Scan 'j' for formal arguments in 'table' and replace them with Args.
    // And scan 'j' for free Args not in 'table' and promote their levels.
    static object ScanForArgs(object j, IDictionary<Sym, Arg> table) {
        switch (j) {
        case Sym sym:
            return ((table.TryGetValue(sym, out Arg a)) ? a :
                    j);
        case Arg arg:
            return ((table.TryGetValue(arg.Symbol, out Arg k)) ? k :
                    new Arg(arg.Level + 1, arg.Offset, arg.Symbol));
        case Cell cell:
            if (cell.Car == QuoteSym)
                return j;
            else if (cell.Car == QuasiquoteSym)
                return new Cell(QuasiquoteSym, 
                                ScanForQQ(cell.Cdr, table, 0));
            else
                return MapCar(cell, x => ScanForArgs(x, table));
        default:
            return j;
        }
    }

    // Scan for quasi-quotes and ScanForArgs them depending on the nesting
    // level.
    static object ScanForQQ(object j, IDictionary<Sym, Arg> table, int level) {
        if (j is Cell cell) {
            var car = cell.Car;
            var cdr = cell.Cdr;
            if (car == QuasiquoteSym) {
                return new Cell(car, ScanForQQ(cdr, table, level + 1));
            } else if (car == UnquoteSym || car == UnquoteSplicingSym) {
                var d = ((level == 0) ? ScanForArgs(cdr, table) :
                         ScanForQQ(cdr, table, level - 1));
                if (d == cdr)
                    return j;
                return new Cell(car, d);
            } else {
                return MapCar(cell, x => ScanForQQ(x, table, level));
            }
        } else {
            return j;
        }
    }


    //------------------------------------------------------------------
    // Quasi-Quotation

    /// <summary>Expand <c>x</c> of any quqsi-quotation <c>`x</c> into
    /// the equivalent S-expression.</summary>
    public static object QqExpand(object x) =>
        QqExpand0(x, 0);        // Begin with the nesting level 0.

    /// <summary>Quote <c>x</c> so that the result evaluates to <c>x</c>.
    /// </summary>
    public static object QqQuote(object x) =>
        (x is Sym || x is Cell) ? new Cell(QuoteSym, new Cell(x, null)) : x;

    static object QqExpand0(object x, int level) {
        if (x is Cell cell) {
            if (cell.Car == UnquoteSym) { // ,a
                if (level == 0)
                    return CdrCell(cell).Car; // ,a => a
            }
            Cell t = QqExpand1(cell, level);
            if ((t.Car is Cell k) && t.Cdr == null) {
                if (k.Car == ListSym || k.Car == ConsSym)
                    return k;
            }
            return new Cell(AppendSym, t);
        } else {
            return QqQuote(x);
        }
    }

    // Expand x of `x so that the result can be used as an argument of append.
    // Example 1: (,a b) => h=(list a) t=((list 'b)) => ((list a 'b))
    // Example 2: (,a ,@(cons 2 3)) => h=(list a) t=((cons 2 3))
    //                              => ((cons a (cons 2 3)))
    static Cell QqExpand1(object x, int level) {
        if (x is Cell cell) {
            if (cell.Car == UnquoteSym) { // ,a
                if (level == 0)
                    return CdrCell(cell); // ,a => (a)
                level--;
            } else if (cell.Car == QuasiquoteSym) { // `a
                level++;
            }
            var h = QqExpand2(cell.Car, level);
            Cell t = QqExpand1(cell.Cdr, level); // != null
            if (t.Car == null && t.Cdr == null) {
                return new Cell(h, null);
            } else if (h is Cell hcell) {
                if (hcell.Car == ListSym) {
                    if (t.Car is Cell tcar) {
                        if (tcar.Car == ListSym) {
                            var hh = QqConcat(hcell, tcar.Cdr);
                            return new Cell(hh, t.Cdr);
                        }
                    }
                    if (hcell.Cdr != null) {
                        var hh = QqConsCons(CdrCell(hcell), t.Car);
                        return new Cell(hh, t.Cdr);
                    }
                }
            }
            return new Cell(h, t);
        } else {
            return new Cell(QqQuote(x), null);
        }
    }

    // (1 2), (3 4) => (1 2 3 4)
    static object QqConcat(Cell x, object y) =>
        (x == null) ? y :
        new Cell(x.Car, QqConcat(CdrCell(x), y));

    // (1 2 3), "a" => (cons 1 (cons 2 (cons 3 "a")))
    static object QqConsCons(Cell x, object y) =>
        (x == null) ? y :
        new Cell(ConsSym,
                 new Cell(x.Car,
                          new Cell(QqConsCons(CdrCell(x), y), null)));

    // Expand x.car of `x so that the result can be used as an arg of append.
    // Example: ,a => (list a); ,@(foo 1 2) => (foo 1 2); b => (list 'b)
    static object QqExpand2(object y, int level) { // Let y be x.car.
        if (y is Cell cell) {
            if (cell.Car == UnquoteSym) { // ,a
                if (level == 0)
                    return new Cell(ListSym, cell.Cdr); // ,a => (list a)
                level--;
            } else if (cell.Car == UnquoteSplicingSym) { // ,@a
                if (level == 0)
                    return CdrCell(cell).Car; // ,@a => a
                level--;
            } else if (cell.Car == QuasiquoteSym) { // `a
                level++;
            }
        }
        return new Cell(ListSym, new Cell(QqExpand0(y, level), null));
    }


    //------------------------------------------------------------------

    /// <summary>Reader of Lisp expressions</summary>
    public class Reader {
        readonly TextReader TReader;
        object Token;
        IEnumerator<string> Tokens =
            ((IEnumerable<string>) new string[0]).GetEnumerator();
        int LineNo = 0;
        string Line = "";
        bool Erred = false;

        /// <summary>Token of "End Of File"</summary>
        public static object EOF = new Sym("#EOF");

        /// <summary>Construct a Lisp reader.</summary>
        /// <param name="tr">Text reader from which Lisp expressions will
        /// be read</param>
         public Reader(TextReader tr) {
             TReader = tr;
         }
 
        /// <summary>Read a Lisp expression and return it.</summary>
        /// <remarks>Return EOF if the input runs out.</remarks>
        public async Task<object> Read() {
            try {
                await ReadToken();
                return await ParseExpression();
            } catch (FormatException ex) {
                throw new EvalException("syntax error",
                                        $"{ex.Message} -- {LineNo}: {Line}",
                                        false);
            }
        }

        async Task<object> ParseExpression() {
            if (Token == LeftParenSym) { // (a b c)
                await ReadToken();
                return await ParseListBody();
            } else if (Token == SingleQuoteSym) { // 'a => (quote a)
                await ReadToken();
                return new Cell(QuoteSym,
                                new Cell(await ParseExpression(), null));
            } else if (Token == BackQuoteSym) { // `a => (quasiquote a)
                await ReadToken();
                return new Cell(QuasiquoteSym,
                                new Cell(await ParseExpression(), null));
            } else if (Token == CommaSym) { // ,a => (unquote a)
                await ReadToken();
                return new Cell(UnquoteSym,
                                new Cell(await ParseExpression(), null));
            } else if (Token == CommaAtSym) { // ,@a => (unquote-splicing a)
                await ReadToken();
                return new Cell(UnquoteSplicingSym,
                                new Cell(await ParseExpression(), null));
            } else if (Token == DotSym || Token == RightParenSym) {
                throw new FormatException($"unexpected {Token}");
            } else {
                return Token;
            }
        }

        async Task<Cell> ParseListBody() {
            if (Token == EOF) {
                throw new FormatException("unexpected EOF");
            } else if (Token == RightParenSym) {
                return null;
            } else {
                var e1 = await ParseExpression();
                await ReadToken();
                object e2;
                if (Token == DotSym) { // (a . b)
                    await ReadToken();
                    e2 = await ParseExpression();
                    await ReadToken();
                    if (Token != RightParenSym) 
                        throw new FormatException($"\")\" expected: {Token}");
                } else {
                    e2 = await ParseListBody();
                }
                return new Cell(e1, e2);
            }
        }

        // Read the next token and set it to Token.
        async Task ReadToken() {
            while (!Tokens.MoveNext() || Erred) { // line ends or erred
                Erred = false;
                LineNo++;
                Line = await TReader.ReadLineAsync();
                if (Line == null) {
                    Token = EOF;
                    return;
                }
                Tokens = ToTypedMatches(TokenPat.Matches(Line))
                    .Select((Match m) => m.Groups[1].Value)
                    .Where((string s) => s != "")
                    .GetEnumerator();
            }
            string t = Tokens.Current;
            if (t[0] == '"') {
                int n = t.Length - 1;
                if (n < 1 || t[n] != '"')
                    throw new FormatException($"bad string: {t}");
                t = t.Substring(1, n - 1);
                t = EscapePat.Replace(t, (Match m) => {
                        String key = m.Groups[1].Value;
                        return (Escapes.ContainsKey(key) ? Escapes[key] :
                                $"\\{key}");
                    });
                Token = t;
                return;
            }
            if (Double.TryParse(t, out double num))
                Token = num;
            else if (t == "nil")
                Token = null;
            else
                Token = Sym.New(t);
        }
    }

    static IEnumerable<Match> ToTypedMatches(MatchCollection matches) {
        foreach (Match match in matches) {
            yield return match;
        }
    }

    // Regular expression to split a line into Lisp tokens
    static readonly Regex TokenPat =
        new Regex(@"\s+|;.*$|(""(\\.?|.)*?""|,@?|[^()'`~""; \t]+|.)");

    // Regular expression to take an escape sequence out of a string
    static readonly Regex EscapePat = new Regex(@"\\(.)");

    // Mapping from a character of escape sequence to its string value
    static readonly Dictionary<string, string> Escapes =
        new Dictionary<string, string> {
        ["\\"] = "\\",
        ["\""] = "\"",
        ["n"] = "\n",
        ["r"] = "\r",
        ["f"] = "\f",
        ["b"] = "\b",
        ["t"] = "\t",
        ["v"] = "\v"
    };


    //------------------------------------------------------------------

    /// <summary>Make a string representation of Lisp expression.</summary>
    /// <param name="x">Lisp expression</param>
    /// <param name="quoteString">flag whether to quote string</param>
    public static string Str(object x, bool quoteString=true) {
        // 4 is the threshold of ellipsis for circular lists
        return Str4(x, quoteString, 4, null);
    }

    // Mapping from a quote symbol to its string representation
    static readonly Dictionary<Sym, string> Quotes =
         new Dictionary<Sym, string> {
        [QuoteSym] = "'",
        [QuasiquoteSym] = "`",
        [UnquoteSym] = ",",
        [UnquoteSplicingSym] = ",@"
    };

    static string Str4(object x, bool quoteString, int count,
                       HashSet<Cell> printed) {
        switch (x) {
        case null:
            return "nil";
        case Cell cell:
            if ((cell.Car is Sym csym) && Quotes.ContainsKey(csym)) {
                if ((cell.Cdr is Cell xcdr) && xcdr.Cdr == null)
                    return Quotes[csym]
                        + Str4(xcdr.Car, true, count, printed);
            }
            return "(" + StrListBody(cell, count, printed) + ")";
        case string st:
            if (! quoteString)
                return st;
            var bf = new StringBuilder();
            bf.Append('"');
            foreach (char ch in st) {
                switch (ch) {
                case '\b': bf.Append(@"\b"); break;
                case '\t': bf.Append(@"\t"); break;
                case '\n': bf.Append(@"\n"); break;
                case '\v': bf.Append(@"\v"); break;
                case '\f': bf.Append(@"\f"); break;
                case '\r': bf.Append(@"\r"); break;
                case '"':  bf.Append("\\\""); break;
                case '\\': bf.Append(@"\\"); break;
                default: bf.Append(ch); break;
                }
            }
            bf.Append('"');
            return bf.ToString();
        case Sym sym:
            return (sym.IsInterned) ? sym.Name : $"#:{x}";
        default:
            return x.ToString();
        }
    }

    // Make a string representation of list omitting its "(" and ")".
    static string StrListBody(Cell x, int count, HashSet<Cell> printed) {
        if (printed == null)
            printed = new HashSet<Cell>();
        var s = new List<string>();
        object y;
        for (y = x; y is Cell cell; y = cell.Cdr) {
            if (printed.Add(cell)) {
                count = 4;
            } else {
                count--;
                if (count < 0) {
                    s.Add("..."); // an ellipsis for a circular list
                    return String.Join(" ", s);
                }
            }
            s.Add(Str4(cell.Car, true, count, printed));
        }
        if (y != null) {
            s.Add(".");
            s.Add(Str4(y, true, count, printed));
        }
        for (y = x; y is Cell cell; y = cell.Cdr)
            printed.Remove(cell);
        return String.Join(" ", s);
    }


    //------------------------------------------------------------------

    /// <summary>Run REPL (Read-Eval-Print Loop).</summary>
    public static async Task Run(Interp interp, TextReader input) {
        bool interactive = (input == null);
        if (interactive)
            input = Console.In;
        var reader = new Reader(input);
        for (;;) {
            if (interactive) {
                Console.Write("> ");
                try {
                    var sExp = await reader.Read();
                    if (sExp == Reader.EOF)
                        return;
                    var x = interp.Eval(sExp, null);
                    Console.WriteLine(Str(x));
                } catch (Exception ex) {
                    Console.WriteLine(ex);
                }
            } else {
                var sExp = await reader.Read();
                if (sExp == Reader.EOF)
                    return;
                interp.Eval(sExp, null);
            }
        }
    }

    /// <summary>Make a Lisp interpreter initialized with Prelude.</summary>
    public static async Task<Interp> MakeInterp() {
        var interp = new Interp();
        await Run(interp, new StringReader(Prelude));
        return interp;
    }

    static int Main(string[] args) {
        var interp = MakeInterp().Result;
        if (args.Length == 0) {
            args = new string[] {"-"};
        }
        foreach (var fileName in args) {
            if (fileName == "-") {
                Run(interp, null).Wait();
                Console.WriteLine("Goodbye");
            } else {
                var input = new StreamReader(fileName);
                Run(interp, input).Wait();
            }
        }
        return 0;
    }


    /// <summary>Lisp initialization script</summary>
    public static readonly string Prelude = @"
(setq defmacro
      (macro (name args &rest body)
             `(progn (setq ,name (macro ,args ,@body))
                     ',name)))

(defmacro defun (name args &rest body)
  `(progn (setq ,name (lambda ,args ,@body))
          ',name))

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))
(defun caaar (x) (car (car (car x))))
(defun caadr (x) (car (car (cdr x))))
(defun cadar (x) (car (cdr (car x))))
(defun caddr (x) (car (cdr (cdr x))))
(defun cdaar (x) (cdr (car (car x))))
(defun cdadr (x) (cdr (car (cdr x))))
(defun cddar (x) (cdr (cdr (car x))))
(defun cdddr (x) (cdr (cdr (cdr x))))
(defun not (x) (eq x nil))
(defun consp (x) (not (atom x)))
(defun print (x) (prin1 x) (terpri) x)
(defun identity (x) x)

(setq
 = eql
 null not
 setcar rplaca
 setcdr rplacd)

(defun > (x y) (< y x))
(defun >= (x y) (not (< x y)))
(defun <= (x y) (not (< y x)))
(defun /= (x y) (not (= x y)))

(defun equal (x y)
  (cond ((atom x) (eql x y))
        ((atom y) nil)
        ((equal (car x) (car y)) (equal (cdr x) (cdr y)))))

(defmacro if (test then &rest else)
  `(cond (,test ,then)
         ,@(cond (else `((t ,@else))))))

(defmacro when (test &rest body)
  `(cond (,test ,@body)))

(defmacro let (args &rest body)
  ((lambda (vars vals)
     (defun vars (x)
       (cond (x (cons (if (atom (car x))
                          (car x)
                        (caar x))
                      (vars (cdr x))))))
     (defun vals (x)
       (cond (x (cons (if (atom (car x))
                          nil
                        (cadar x))
                      (vals (cdr x))))))
     `((lambda ,(vars args) ,@body) ,@(vals args)))
   nil nil))

(defmacro letrec (args &rest body)      ; (letrec ((v e) ...) body...)
  (let (vars setqs)
    (defun vars (x)
      (cond (x (cons (caar x)
                     (vars (cdr x))))))
    (defun sets (x)
      (cond (x (cons `(setq ,(caar x) ,(cadar x))
                     (sets (cdr x))))))
    `(let ,(vars args) ,@(sets args) ,@body)))

(defun _append (x y)
  (if (null x)
      y
    (cons (car x) (_append (cdr x) y))))
(defmacro append (x &rest y)
  (if (null y)
      x
    `(_append ,x (append ,@y))))

(defmacro and (x &rest y)
  (if (null y)
      x
    `(cond (,x (and ,@y)))))

(defun mapcar (f x)
  (and x (cons (f (car x)) (mapcar f (cdr x)))))

(defmacro or (x &rest y)
  (if (null y)
      x
    `(cond (,x)
           ((or ,@y)))))

(defun listp (x)
  (or (null x) (consp x)))    ; NB (listp (lambda (x) (+ x 1))) => nil

(defun memq (key x)
  (cond ((null x) nil)
        ((eq key (car x)) x)
        (t (memq key (cdr x)))))

(defun member (key x)
  (cond ((null x) nil)
        ((equal key (car x)) x)
        (t (member key (cdr x)))))

(defun assq (key alist)
  (cond (alist (let ((e (car alist)))
                 (if (and (consp e) (eq key (car e)))
                     e
                   (assq key (cdr alist)))))))

(defun assoc (key alist)
  (cond (alist (let ((e (car alist)))
                 (if (and (consp e) (equal key (car e)))
                     e
                   (assoc key (cdr alist)))))))

(defun _nreverse (x prev)
  (let ((next (cdr x)))
    (setcdr x prev)
    (if (null next)
        x
      (_nreverse next x))))
(defun nreverse (list)            ; (nreverse '(a b c d)) => (d c b a)
  (cond (list (_nreverse list nil))))

(defun last (list)
  (if (atom (cdr list))
      list
    (last (cdr list))))

(defun nconc (&rest lists)
  (if (null (cdr lists))
      (car lists)
    (if (null (car lists))
        (apply nconc (cdr lists))
      (setcdr (last (car lists))
              (apply nconc (cdr lists)))
      (car lists))))

(defmacro while (test &rest body)
  (let ((loop (gensym)))
    `(letrec ((,loop (lambda () (cond (,test ,@body (,loop))))))
       (,loop))))

(defmacro dolist (spec &rest body) ; (dolist (name list [result]) body...)
  (let ((name (car spec))
        (list (gensym)))
    `(let (,name
           (,list ,(cadr spec)))
       (while ,list
         (setq ,name (car ,list))
         ,@body
         (setq ,list (cdr ,list)))
       ,@(if (cddr spec)
             `((setq ,name nil)
               ,(caddr spec))))))

(defmacro dotimes (spec &rest body) ; (dotimes (name count [result]) body...)
  (let ((name (car spec))
        (count (gensym)))
    `(let ((,name 0)
           (,count ,(cadr spec)))
       (while (< ,name ,count)
         ,@body
         (setq ,name (+ ,name 1)))
       ,@(if (cddr spec)
             `(,(caddr spec))))))
";
}

/*
  Copyright (c) 2017 OKI Software Co., Ltd.
  Copyright (c) 2018 SUZUKI Hisao

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.
*/
