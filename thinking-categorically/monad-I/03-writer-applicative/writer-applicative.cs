using System;
using System.Collections.Generic;

public class Writer<A>
{
    public A Value { get; }
    public List<string> Log { get; }

    public Writer(A value, List<string> log)
    {
        Value = value;
        Log = log;
    }

    public static Writer<A> Pure(A value) => new Writer<A>(value, new List<string>());

    public static Writer<B> Apply<B>(Writer<Func<A, B>> wf, Writer<A> wa)
    {
        var combinedLog = new List<string>(wf.Log);
        combinedLog.AddRange(wa.Log);
        return new Writer<B>(wf.Value(wa.Value), combinedLog);
    }
}

class Program
{
    static Writer<int> AddLog(int x) => new Writer<int>(x, new List<string> { $"Added {x}" });

    static void Main()
    {
        var w1 = AddLog(3);
        var w2 = AddLog(5);

        // Create a function that takes two integers and adds them
        var addTwoNumbers = new Writer<Func<int, int>>(x => x + 5, new List<string> { "Created add function" });
        var result = Writer<int>.Apply<int>(addTwoNumbers, w1);
        Console.WriteLine(result.Value); // 8
        Console.WriteLine(string.Join(", ", result.Log)); // Created add function, Added 3

        // More complex example: manual currying simulation
        var w1PlusFive = AddLog(3);
        var w2PlusFive = AddLog(5);
        var addFunc = new Writer<Func<int, int>>(x => x + w2PlusFive.Value, w2PlusFive.Log);
        var finalResult = Writer<int>.Apply<int>(addFunc, w1PlusFive);
        Console.WriteLine(finalResult.Value); // 8 (3 + 5)
        Console.WriteLine(string.Join(", ", finalResult.Log)); // Added 5, Added 3
    }
}
