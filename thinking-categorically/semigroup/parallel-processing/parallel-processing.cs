using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

public interface ISemigroup<T>
{
    T Combine(T a, T b);
}

// Semigroup implementation for sum
public class SumSemigroup : ISemigroup<long>
{
    public long Combine(long a, long b) => a + b;
}

// Parallel processor using TPL
public class ParallelProcessor<T>
{
    private readonly ISemigroup<T> _semigroup;

    public ParallelProcessor(ISemigroup<T> semigroup)
    {
        _semigroup = semigroup;
    }

    // Parallel reduce using divide-and-conquer
    public async Task<T?> ParallelReduceAsync(IEnumerable<T> data)
    {
        var list = data.ToList();
        if (!list.Any()) return default(T);
        if (list.Count == 1) return list[0];

        return await Task.Run(() => ParallelReduceInternal(list));
    }

    private T ParallelReduceInternal(IList<T> data)
    {
        if (data.Count == 1) return data[0];
        if (data.Count == 2) return _semigroup.Combine(data[0], data[1]);

        // Split into chunks for parallel processing
        var mid = data.Count / 2;
        var left = data.Take(mid).ToList();
        var right = data.Skip(mid).ToList();

        // Process both halves in parallel
        var leftTask = Task.Run(() => ParallelReduceInternal(left));
        var rightTask = Task.Run(() => ParallelReduceInternal(right));

        Task.WaitAll(leftTask, rightTask);

        return _semigroup.Combine(leftTask.Result, rightTask.Result);
    }

    // Map-reduce with parallel processing
    public async Task<T?> ParallelMapReduceAsync<U>(
        IEnumerable<U> data,
        Func<U, T> mapper)
    {
        var list = data.ToList();
        if (!list.Any()) return default(T);

        // Determine optimal chunk size
        var processorCount = Environment.ProcessorCount;
        var chunkSize = Math.Max(1, list.Count / processorCount);

        // Split into chunks
        var chunks = list
            .Select((item, index) => new { item, index })
            .GroupBy(x => x.index / chunkSize)
            .Select(g => g.Select(x => x.item).ToList())
            .ToList();

        // Process chunks in parallel
        var chunkTasks = chunks.Select(chunk => Task.Run(() =>
        {
            var mapped = chunk.Select(mapper);
            return mapped.Aggregate(_semigroup.Combine);
        }));

        var chunkResults = await Task.WhenAll(chunkTasks);

        // Combine chunk results
        return chunkResults.Aggregate(_semigroup.Combine);
    }

    // PLINQ-based parallel processing
    public T? PLinqMapReduce<U>(IEnumerable<U> data, Func<U, T> mapper)
    {
        return data.AsParallel()
                  .Select(mapper)
                  .Aggregate(_semigroup.Combine);
    }
}

// Performance demonstration
class Program
{
    static async Task Main(string[] args)
    {
        var numbers = Enumerable.Range(1, 10_000).ToList();
        // Parallel sum processing
        var sumProcessor = new ParallelProcessor<long>(new SumSemigroup());

        var parallelSum = await sumProcessor.ParallelMapReduceAsync(numbers, x => (long)x);

        Console.WriteLine($"Parallel Sum: {parallelSum}");

        // PLINQ comparison
        var plinqSum = sumProcessor.PLinqMapReduce(numbers, x => (long)x);

        Console.WriteLine($"PLINQ Sum: {plinqSum}");

        // Sequential comparison
        var sequentialSum = numbers.Select(x => (long)x).Aggregate((a, b) => a + b);
        Console.WriteLine($"Sequential Sum: {sequentialSum}");

        // Demonstrate associativity in parallel context
        Console.WriteLine("\nDemonstrating Associativity in Parallel Processing:");

        var smallNumbers = new[] { 1, 2, 3, 4, 5, 6, 7, 8 };
        var result1 = await sumProcessor.ParallelReduceAsync(smallNumbers.Select(x => (long)x));
        var result2 = smallNumbers.Select(x => (long)x).Aggregate((a, b) => a + b);

        Console.WriteLine($"Parallel result: {result1}");
        Console.WriteLine($"Sequential result: {result2}");
        Console.WriteLine($"Results match: {result1 == result2}");
    }
}
