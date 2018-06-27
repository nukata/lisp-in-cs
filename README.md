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
$ dotnet build
....
$ dotnet bin/Debug/netcoreapp2.1/lisp.dll
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


## License

This is under the MIT License.
See [`lisp.cs L1393-L1414`](lisp.cs#L1393-L1414).
