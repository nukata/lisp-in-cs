# Lisp in C# 7

This is a Lisp interpreter compatible with
[lisp-in-dart](https://github.com/nukata/lisp-in-dart)
~~except for numeric types:  all numbers are `double` in C#~~.
I wrote it in C# 6 and presented it under the MIT License at
<http://www.oki-osk.jp/esc/cs/lisp.html> (broken link)
until the spring of 2017 (H29).
I slightly modified it to match C# 7 in 2018 (H30).

Now in 2019 (R1),
I implemented a mixed mode arithmetic of `int`, `double` and `BigInteger`
in the same way as
[little-scheme-in-cs](https://github.com/nukata/little-scheme-in-cs).



The same as lisp-in-dart, [lisp-in-go](https://github.com/nukata/lisp-in-go)
and [lisp-in-typescript](https://github.com/nukata/lisp-in-typescript),
this is a Lisp-1 with tail call optimization
and partially hygienic macros but being a subset of Common Lisp
in a loose meaning.
It is easy to write a nontrivial script which runs both in this and in
Common Lisp (and also in Emacs Lisp).
Examples are found in the [`examples`](examples) folder.

See [IMPLEMENTATION-NOTES.md](IMPLEMENTATION-NOTES.md) for the implementation.


## How to run

With [Mono](https://www.mono-project.com) 6.12.0:

```
$ csc -o -r:System.Numerics.dll lisp.cs arith.cs
....
$ mono lisp.exe
> (+ 5 6)
11
> (exit 0)
$ 
```

With [.NET](https://github.com/dotnet/core) 6.0:

```
$ dotnet build -c Release
....
$ ./bin/Release/net6.0/lisp
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


## Examples

There are five files ending with `.l` under the `examples` folder.
These run also in Emacs Lisp and Common Lisp.

- [`qsort.l`](examples/qsort.l)
  performs a quick sort.

```
$ mono lisp.exe examples/qsort.l
(1 1 2 3 3 4 5 5 5 6 7 8 9 9 9)
$ 
```

```
$ emacs -batch -l examples/qsort.l

(1 1 2 3 3 4 5 5 5 6 7 8 9 9 9)
$ 
```

```
$ clisp examples/qsort.l

(1 1 2 3 3 4 5 5 5 6 7 8 9 9 9)
$ 
```


- [`fact100.l`](examples/fact100.l)
  calculates 100!.

```
$ mono lisp.exe examples/fact100.l 
93326215443944152681699238856266700490715968264381621468592963895217599993229915
608941463976156518286253697920827223758251185210916864000000000000000000000000
$
```

- [`fib15.l`](examples/fib15.l)
  calculates Fibonacci for 15.

- [`eval-fib15.l`](examples/eval-fib15.l)
  calculates Fibonacci for 15 on a meta-circular Lisp evaluator.

- [`eval-eval-fib15.l`](examples/eval-eval-fib15.l)
  calculates Fibonacci for 15 on a meta-circular Lisp evaluator 
  on a meta-circular Lisp evaluator.



There is one more example:

- [`interp_in_thread.cs`](examples/interp_in_thread.cs)
  runs a Lisp interpreter in another thread.
  You can embed an interpreter within your application in the same way.

```
$ cd examples
$ csc -o -t:library -r:System.Numerics.dll ../lisp.cs ../arith.cs
....
$ csc -r:lisp.dll interp_in_thread.cs
...
$ mono interp_in_thread.exe
=> (1 . 2)
Reiwa
=> Reiwa
$ 
```

The examples of `eval-fib15.l` and `eval-eval-fib15.l` are inspired 
by <https://github.com/zick/ZickStandardLisp>.



## Performance

The following is a result of a benchmark test: the time to execute [`eval-eval-fib15.l`](examples/eval-eval-fib15.l).
I used MacBook Pro (15-inch, 2016), 2.6GHz Core i7, 16GB 2133MHz LPDDR3, macOS Mojave 10.14.6.

| Lisp                                                                          | Compiled/Executed on                                            | Executed in   | Executes    | Time [sec] | Rel. Speed  |
|:------------------------------------------------------------------------------|:----------------------------------------------------------------|:--------------|:------------|-----------:|------------:| 
| GNU CLISP 2.49                                                                |                                                                 | Mach-O        | *.fas       |     4.0    | 8.0
| GNU Emacs Lisp 26.2                                                           |                                                                 | Mach-O        | *.elc       |     6.4    | 5.0
| [l2lisp-in-java](https://github.com/nukata/l2lisp-in-java) 1.0.0-9.4          | [AdoptOpenJDK 11.0.5+10 HotSpot](http://adoptopenjdk.net/)      | *.jar         | source file |    12.8    | 2.5
| GNU Emacs Lisp 26.2                                                           |                                                                 | Mach-O        | source file |    16.2    | 2.0
| [lisp-in-dart](https://github.com/nukata/lisp-in-dart) 1.0.1                  | Dart VM 2.5.2                                                   | snapshot      | source file |    20.4    | 1.6
| [lisp-in-dart](https://github.com/nukata/lisp-in-dart) 1.0.1                  | /Dart VM 2.5.2                                                  | source file   | source file |    21.4    | 1.5
| [lisp-in-typescript](https://github.com/nukata/lisp-in-typescript) 1.0.0-1.27 | TS 3.6.4/Node.js 12.12.0                                        | *.js (ESNEXT) | source file |    23.9    | 1.3
| [lisp-in-typescript](https://github.com/nukata/lisp-in-typescript) 1.0.0-1.27 | TS 3.6.4/Node.js 12.12.0                                        | *.js (ES5)    | source file |    25.4    | 1.3
| lisp-in-cs 2.0.0                                                              | .NET Core SDK 3.0.100                                           | *.dll (.NET)  | source file |    31.8    | 1.0
| [l2lisp-in-python](https://github.com/nukata/l2lisp-in-python) (7.2)          | /PyPy 7.1.1(Python 3.6.1)                                       | source file   | source file |    37.8    | 0.8
| [l2lisp-in-python](https://github.com/nukata/l2lisp-in-python) (7.2)          | /PyPy 7.1.1(Python 2.7.13)                                      | source file   | source file |    41.7    | 0.8
| lisp-in-cs 2.0.0                                                              | Mono 6.4.0.198                                                  | *.exe (.NET)  | source file |    43.9    | 0.7
| [lisp-in-go](https://github.com/nukata/lisp-in-go) 2.0.1                      | Go 1.13.3/                                                      | Mach-O        | source file |    66.6    | 0.5
| GNU CLISP 2.49                                                                |                                                                 | Mach-O        | source file |   575.8    | 0.1
| [l2lisp-in-python](https://github.com/nukata/l2lisp-in-python) (7.2)          | /Python 3.7.4                                                   | source file   | source file |  1116.7    | 0.0

I am sorry to say that the performance of this Lisp (lisp-in-cs) is rather mediocre.
Note that l2lisp-in-java, lisp-in-dart, lisp-in-typescript, lisp-in-cs and lisp-in-go are all written in largely the same way; l2lisp-in-python is a little old-fashioned.
Therefore, *roughly speaking*, their speeds shown above reflect those of their respective implementation languages: Java, Dart, TypeScript, C# and Go (and Python).

