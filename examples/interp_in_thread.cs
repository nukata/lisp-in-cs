// An example of running Lisp in another thread
using System;
using System.Collections.Concurrent;
using System.IO;
using System.Text;
using System.Threading;

// csc -o -t:library -r:System.Numerics.dll ../lisp.cs ../arith.cs 
// csc -r:lisp.dll interp_in_thread.cs
// mono interp_in_thread.exe

// Expected output:
// => (1 . 2)
// Reiwa
// => Reiwa

public static class ThreadTest {

    // A simple substitute for Console.Out
    class SendOut: TextWriter {
        public BlockingCollection<object> Queue
            = new BlockingCollection<object>();

        public override Encoding Encoding => Encoding.UTF8;
        public override void Write(char value) {
            Queue.Add(value);
        }
    }

    static readonly string EndSentinel = ":END";

    // A Read-Eval-Send Loop in another thread
    static void RESLoop(SendOut so, BlockingCollection<string> receiveIn) {
        var interp = NukataLisp.MakeInterp().Result;
        interp.COut = so;
        for (;;) {
            string s = receiveIn.Take();
            if (s == EndSentinel)
                break;
            object x = NukataLisp.Run(interp, new StringReader(s)).Result;
            so.Queue.Add(x);
            so.Queue.Add(EndSentinel);
        }
        so.Queue.Add(EndSentinel);
    }

    // Run Lisp in another thread and send it S-expression strings.
    static void Main(string[] args) {
        SendOut so = new SendOut();
        var queue = new BlockingCollection<string>();
        new Thread(() => RESLoop(so, queue)).Start();
        foreach (string sExpression in new string[]{
                "(cons 1 2)",
                "(print 'Reiwa)",
                EndSentinel
            }) {
            queue.Add(sExpression);
            for (;;) {
                object x = so.Queue.Take();
                if (x is string s && s == EndSentinel) {
                    break;
                } else if (x is char ch) {
                    Console.Write(ch);
                } else {
                    Console.WriteLine("=> {0}", x);
                }
            }
        }
    }
}
