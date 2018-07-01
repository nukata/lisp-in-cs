# Lisp in C# 7

This is a Lisp interpreter compatible with
[lisp-in-dart](https://github.com/nukata/lisp-in-dart)
except for numeric types:  all numbers are `double` in C#.
I wrote it in C# 6 and presented it under the MIT License at
<http://www.oki-osk.jp/esc/cs/lisp.html> (broken link)
until the spring of 2017 (H29).
Now I have slightly modified it to match C# 7.

The same as lisp-in-dart, [lisp-in-go](https://github.com/nukata/lisp-in-go)
and [lisp-in-typescript](https://github.com/nukata/lisp-in-typescript),
this is a Lisp-1 with tail call optimization
and partially hygienic macros but being a subset of Common Lisp
in a loose meaning.
It is easy to write a nontrivial script which runs both in this and in
Common Lisp (and also in Emacs Lisp).
Examples are found in the [`examples`](examples) folder copied from
[lisp-in-dart/examples](http://github.com/nukata/lisp-in-dart/tree/master/examples).

See [IMPLEMENTATION-NOTES.md](IMPLEMENTATION-NOTES.md) for the implementation.


## How to run

Use Mono 5.12 or .NET Core 2.1 (or some compatible ones ;)
to compile [`lisp.cs`](lisp.cs).

```
$ csc /o lisp.cs
....
$ mono lisp.exe
> (+ 5 6)
11
> (exit 0)
$ 
```

```
$ mv examples/interp_in_thread.cs examples/interp_in_thread.cs~
$ dotnet build -c Release
....
$ dotnet bin/Release/netcoreapp2.1/lisp.dll
> (+ 5 6)
11
> (exit 0)
$
```

You can give it a file name of your Lisp script.
If you put a "`-`" after the file name, it will
begin an interactive session after running the file.

```
$ cat examples/fib15.l
(defun fib (n)
  (if (< n 2)
      1
    (+ (fib (- n 1))
       (fib (- n 2)))))
(print (fib 15))
$ mono lisp.exe examples/fib15.l -
987
> (fib 0)
1
> (fib 15)
987
> (fib 16)
1597
> (exit 0)
$ 
```


## Performance

The following is a result of my benchmark test inspired by <http://github.com/zick/ZickStandardLisp>:
the time to execute [`examples/eval-eval-fib15.l`](examples/eval-eval-fib15.l)
which calculates Fibonacci for 15 on a meta-circular Lisp evaluator on a meta-circular Lisp evaluator.
I used MacBook Pro (15-inch, 2016), 2.6GHz Core i7, 16GB 2133MHz LPDDR3, macOS High Sierra 10.13.5.

| Lisp                                                                          | Compiled/Executed on                                            | Executed in   | Executes    | Time [sec] | Rel. Speed  |
|:------------------------------------------------------------------------------|:----------------------------------------------------------------|:--------------|:------------|-----------:|------------:| 
| GNU CLISP 2.49                                                                |                                                                 | Mach-O        | *.fast      |     4.3    | 8.23
| GNU Emacs Lisp 22.1                                                           |                                                                 | Mach-O        | *.elc       |     6.5    | 5.45
| [l2lisp-in-java](https://github.com/nukata/l2lisp-in-java) 1.0.0-9.4          | [OpenJDK 10.0.1](http://jdk.java.net/10/)                       | *.jar         | source file |    13.4    | 2.64
| GNU Emacs Lisp 22.1                                                           |                                                                 | Mach-O        | source file |    14.4    | 2.46
| [l2lisp-in-java](https://github.com/nukata/l2lisp-in-java) 1.0.0-9.4          | [OpenJDK 1.8.0_152(1248.6)](https://github.com/JetBrains/jdk8u) | *.jar         | source file |    17.1    | 2.07
| [lisp-in-dart](https://github.com/nukata/lisp-in-dart) 1.0.0                  | /Dart VM 2.0.0-dev.66.0                                         | source file   | source file |    24.3    | 1.46
| [lisp-in-typescript](https://github.com/nukata/lisp-in-typescript) 1.0.0-1.27 | TS 2.9.2/Node.js 8.11.3                                         | *.js (ES5)    | source file |    26.4    | 1.34
| [lisp-in-typescript](https://github.com/nukata/lisp-in-typescript) 1.0.0-1.27 | TS 2.9.2/Node.js 10.5.0                                         | *.js (ES2017) | source file |    27.6    | 1.28
| [lisp-in-typescript](https://github.com/nukata/lisp-in-typescript) 1.0.0-1.27 | TS 2.9.2/Node.js 10.5.0                                         | *.js (ES5)    | source file |    28.1    | 1.26
| lisp-in-cs 1.0.0-1.22                                                         | .NET Core SDK 2.1.301                                           | *.dll (.NET)  | source file |    35.4    | 1.00
| [l2lisp-in-python](https://github.com/nukata/l2lisp-in-python) (7.2)          | /PyPy 6.0.0(Python 2.7.13)                                      | source file   | source file |    41.5    | 0.85
| lisp-in-cs 1.0.0-1.22                                                         | Mono 5.12.0.226                                                 | *.exe (.NET)  | source file |    42.1    | 0.84
| [lisp-in-typescript](https://github.com/nukata/lisp-in-typescript) 1.0.0-1.27 | TS 2.9.2/Node.js 8.11.3                                         | *.js (ES2017) | source file |    43.7    | 0.81
| [lisp-in-go](https://github.com/nukata/lisp-in-go) 1.0.0-1.42                 | Go 1.10.3/                                                      | Mach-O        | source file |    79.2    | 0.45
| GNU CLISP 2.49                                                                |                                                                 | Mach-O        | source file |  1034.6    | 0.03
| [l2lisp-in-python](https://github.com/nukata/l2lisp-in-python) (7.2)          | /Python 3.7.0                                                   | source file   | source file |  1305.0    | 0.03
| [l2lisp-in-python](https://github.com/nukata/l2lisp-in-python) (7.2)          | /Python 2.7.15                                                  | source file   | source file |  1427.1    | 0.02

I am sorry to say that the performance of this Lisp (lisp-in-cs) is rather mediocre.
Note that l2lisp-in-java, lisp-in-dart, lisp-in-typescript, lisp-in-cs and lisp-in-go are all written in the same way; l2lisp-in-python is a little old-fashioned.
*Roughly speaking*, their speeds shown above reflect their respective implementation languages: Java, Dart, TypeScript, C# and Go (and Python).


## License

This is under the MIT License.
See [`lisp.cs L1393-L1414`](lisp.cs#L1393-L1414).
