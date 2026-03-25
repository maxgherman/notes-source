using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        Console.WriteLine("=== Applicative Identity Law ===\n");

        // Task<T> example (like Promise)
        var taskResult = await TaskIdentityExample();
        Console.WriteLine($"Task<T> result: {taskResult}"); // hello

        // List<T> example (like Array)
        var listResult = ListIdentityExample();
        Console.WriteLine($"List<T> result: {string.Join(", ", listResult)}"); // 1, 2, 3
    }

    // Identity function
    static T Identity<T>(T x) => x;

    // -------------------------------
    // 1. Task<T> (Promise-like)
    // -------------------------------
    static async Task<string> TaskIdentityExample()
    {
        var taskValue = Task.FromResult("hello");
        var taskIdentity = Task.FromResult<Func<string, string>>(Identity);

        return await Ap(taskIdentity, taskValue);
    }

    static async Task<TResult> Ap<T, TResult>(
        Task<Func<T, TResult>> tf,
        Task<T> tx)
    {
        var f = await tf;
        var x = await tx;
        return f(x);
    }

    // -------------------------------
    // 2. List<T> (Array-like)
    // -------------------------------
    static List<int> ListIdentityExample()
    {
        var values = new List<int> { 1, 2, 3 };
        var funcs = new List<Func<int, int>> { Identity };

        return Ap(funcs, values);
    }

    static List<TResult> Ap<T, TResult>(
        List<Func<T, TResult>> fs,
        List<T> xs)
    {
        return fs.SelectMany(f => xs.Select(f)).ToList();
    }
}
