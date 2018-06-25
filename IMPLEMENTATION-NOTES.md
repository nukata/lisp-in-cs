# Implementation Notes


<a name="1"></a>
## 1. Overview

The Lisp implementation of [lisp.cs](lisp.cs) is a translation of lisp.dart 
at [lisp-in-dart](https://github.com/nukata/lisp-in-dart) into C# 7.
For simplicity, lisp.cs restricts numbers in Lisp to `double` only.
Below is an example of running lisp.cs with Mono 5.12 on macOS 10.11.

```
$ csc lisp.cs
Microsoft (R) Visual C# Compiler version 2.6.0.62309 (d3f6b8e7)
Copyright (C) Microsoft Corporation. All rights reserved.

$ mono lisp.exe
> (+ 5 6)
11
> `(a b ,(car '(c d)))
(a b c)
> (let ((x '(a b c d)))
     (setcar (cddr x) x)
     x)
(a b (a b (a ...) d) d)
> (princ "\t789\n")
        789
"\t789\n"
> (dump)
(dotimes dolist while nconc last nreverse _nreverse assoc assq member memq listp
 or mapcar and append _append letrec let when if equal /= <= >= > setcdr setcar 
null = identity print consp not cdddr cddar cdadr cdaar caddr cadar caadr caaar 
cddr cdar cadr caar defun defmacro *version* dump exit apply symbol-name intern 
make-symbol gensym *gensym-counter* terpri princ prin1 truncate / - * + mod % < 
eql numberp stringp length rplacd rplaca list eq atom cons cdr car t)
> let
#<macro:-2:((#<lambda:2:((progn (setq #0:0:vars #<lambda:1:((cond (#0:0:x (cons 
(cond ((atom (car #0:0:x)) (car #0:0:x)) (t (caar #0:0:x))) (#1:0:vars (cdr #0:0
:x))))))>) '#0:0:vars) (progn (setq #0:1:vals #<lambda:1:((cond (#0:0:x (cons (c
ond ((atom (car #0:0:x)) nil) (t (cadar #0:0:x))) (#1:1:vals (cdr #0:0:x))))))>)
 '#0:1:vals) (cons (cons 'lambda (cons (#0:0:vars #1:0:args) #1:1:body)) (#0:1:v
als #1:0:args)))> nil nil))>
> (exit 0)
$ 
```

Some features of lisp.cs and lisp.dart are

- It is basically a subset of Emacs Lisp.
  However, it is a Lisp-1 with static scoping.
  In short, it is a _Common Lisp-like Lisp-1_.

- It makes proper tail calls always.

- A quasi-quotation with backquote will be expanded when macros are expanded.

- A circular list is printed with `...` finitely.

- As an escape sequence within strings, you can use any of
  `\"`, `\\`, `\n`, `\r`, `\f`, `\b`, `\t`, `\v`.

- `(dump)` returns a list of all global variables.
  The list does not include special forms such as `lambda` and `setq`
  since they are not variables.

- `*version*` is a three-element list: 
  the (internal) version number, the implementing language, 
  and the name of implementation.

- (`macro` _args_ _body_) is a special form that evaluates to a sort of
  anonymous function, or _macro expression_.
  The global environment will be used whenever (`macro` ...) evaluates.
  When you apply the resultant macro expression to a list of actual arguments,
  the arguments will not be evaluated and the result of the application
  will be evaluated again.
  Thus a variable bound to a macro expression works as a _macro_.

- `defmacro` is a macro which binds a variable to a macro expression.

- `defun` is a macro which binds a variable to a lambda expression.

- `let` is a macro which applies a lambda expression to a list of initial
  values of variables.

- Macros are _partially hygienic_.
  Free symbols within a macro expression will not be captured when the
  expression is applied (i.e., when the macro is expanded).


A restriction of lisp.cs compared to lisp.dart is


- All numbers are represented by double precision floating point numbers
  (`double` in C#).


That is why I call it _Light_.
Maybe I should call it _Lite_, but I followed the usage of _Light_
in the famous [Caml Light](http://caml.inria.fr/caml-light/).

```
> *version*
(1.22 "C# 7" "Nukata Lisp Light")
> 
```


The macro `let` is defined in the prelude as follows.

```Lisp
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
```


Being _partially hygienic_, macros can avoid variable captures,
provided that you always use the result of `(gensym)` for any symbol
newly introduced to the expansion result.
For example:

```Lisp
(defmacro while (test &rest body)
  (let ((loop (gensym)))
    `(letrec ((,loop (lambda () (cond (,test ,@body (,loop))))))
       (,loop))))
```

See [lisp-in-dart/IMPLEMENTATION-NOTES §5](https://github.com/nukata/lisp-in-dart/blob/master/IMPLEMENTATION-NOTES.md#5) for details.

----------------------------------------

**Note:**
I believe partially hygienic macros have ideal usefulness.
If you like, you can define
[anaphoric macros (Japanese page)](http://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/anaphoricMacros.html)
by introducing a symbol (`it` in the following example) to the 
expansion result intentionally without `(gensym)`.

```
> (defmacro aif (test then else)
    `(let ((it ,test))
       (if it ,then ,else) ))
aif
> (aif (+ 7 8 9)
     (print it)
    (print "?"))
24
24
> 
```

----------------------------------------


<a name="2"></a>
## 2. Internal Data Representation

To represent data of the implemented language (Lisp), native types of the 
implementing language (C#) are used as they are, if possible.
They are all treated as `object` uniformly.


| Lisp Expression                     | Internal Representation                |
|:------------------------------------|:---------------------------------------|
| numbers `1`, `2.3`                  | `double`                               |
| strings `"abc"`, `"hello!\n"`       | `string`                               |
| `t`                                 | `Sym` (user-defined)                   |
| `nil`                               | `null`                                 |
| symbols `x`, `+`                    | `Sym` (user-defined)                   |
| keywords `lambda`, `cond`           | `Keyword` (derived from `Sym`)         |
| lists `(x 1 "2")`, `(y . 3)`        | `Cell` (user-defined)                  |

Below is the definition of the `Cell` class.

```CS
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
```


Below is the definition of the `Sym` class.

```CS
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
```

Keywords of Lisp are defined as follows.

```CS
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
```

Now, if you call `Sym.New("cond")` in `ReadToken()`,
you will get `CondSym`, an instance of the `Keyword` class.


<a name="3"></a>
## 3. Implementations of Lisp functions

The `Interp` class implements the core of the Lisp interpreter.
It has a map for global variables and standard out for built-in functions.

```CS
    /// <summary>Core of the Lisp interpreter</summary>
    public class Interp {
        /// <summary>Table of the global values of symbols</summary>
        protected readonly Dictionary<Sym, object> Globals =
            new Dictionary<Sym, object>();

        /// <summary>Standard out</summary>
        public TextWriter COut { get; set; } = Console.Out;
```

Each built-in function is defined with the `Def` method below.
The `carity` argument takes the arity of the function to be defined.
If the function has `&rest`, the `carity` 
takes `-(`_number of fixed arguments_ ` + 1)`.

```CS
        /// <summary>Define a built-in function by a name, an arity,
        /// and a body.</summary>
        public void Def(string name, int carity, BuiltInFuncBody body) {
            Globals[Sym.New(name)] = new BuiltInFunc(name, carity, body);
        }
```

Below is an excerpt of the constructor of `Interp`.
It shows the implementation of five elementary functions of Lisp.

```CS
        /// <summary>Set each built-in function/variable as the global value
        /// of symbol.</summary>
        public Interp() {
            Globals[TSym] = TSym;
            Def("car", 1, a => (a[0] as Cell)?.Car);
            Def("cdr", 1, a => (a[0] as Cell)?.Cdr);
            Def("cons", 2, a => new Cell(a[0], a[1]));
            Def("atom", 1, a => (a[0] is Cell) ? null : TSym);
            Def("eq", 2, a => (a[0] == a[1]) ? TSym : null);
```

The standard out `COut` is used as follows:

```CS
            Def("prin1", 1, a => {
                    COut.Write(Str(a[0], true)); return a[0];
                });
            Def("princ", 1, a => {
                    COut.Write(Str(a[0], false)); return a[0];
                });
            Def("terpri", 0, a => {
                    COut.WriteLine(); return TSym;
                });
```

The function `dump` takes no arguments and returns a list of all global
variables.
Internally it reads the keys from `Globals` and constructs a list of them.

```CS
            Def("dump", 0, a =>
                Globals.Keys.Aggregate((Cell) null, (x, y) => new Cell(y, x)));
```

For an example of running `(dump)`, see [§1](#1).


Several functions and macros of Lisp are defined in the initialization script
`Prelude`, which runs in the task of `MakeInterp`.

```CS
    /// <summary>Make a Lisp interpreter initialized with Prelude.</summary>
    public static async Task<Interp> MakeInterp() {
        var interp = new Interp();
        await Run(interp, new StringReader(Prelude));
        return interp;
    }
```

Below is the head of `Prelude`.

```CS
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
```
