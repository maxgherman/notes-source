using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

// Enhanced monoid interface
public interface IMonoid<T>
{
    T Identity { get; }
    T Combine(T a, T b);
}

public class IntAddMonoid : IMonoid<int>
{
    public int Identity => 0;
    public int Combine(int a, int b) => a + b;
}

// Robust parallel processor with monoid guarantees
public class MonoidParallelProcessor<T>
{
    private readonly IMonoid<T> _monoid;

    public MonoidParallelProcessor(IMonoid<T> monoid)
    {
        _monoid = monoid;
    }

    // Safe parallel fold - always returns a value, never null
    public async Task<T> ParallelFoldAsync(IEnumerable<T> data)
    {
        var list = data.ToList();
        if (!list.Any()) return _monoid.Identity;  // Safe for empty collections!

        return await Task.Run(() => ParallelFoldInternal(list));
    }

    private T ParallelFoldInternal(IList<T> data)
    {
        if (!data.Any()) return _monoid.Identity;
        if (data.Count == 1) return data[0];

        var tasks = new List<Task<T>>();
        var chunkSize = Math.Max(1, data.Count / Environment.ProcessorCount);

        for (int i = 0; i < data.Count; i += chunkSize)
        {
            var chunk = data.Skip(i).Take(chunkSize).ToList();
            tasks.Add(Task.Run(() => chunk.Aggregate(_monoid.Identity, _monoid.Combine)));
        }

        Task.WaitAll(tasks.ToArray());
        return tasks.Select(t => t.Result).Aggregate(_monoid.Identity, _monoid.Combine);
    }

    // Enhanced map-fold with automatic load balancing
    public async Task<T> ParallelMapFoldAsync<U>(
        IEnumerable<U> data,
        Func<U, T> mapper,
        int? chunkSizeOverride = null)
    {
        var list = data.ToList();
        if (!list.Any()) return _monoid.Identity;

        var chunkSize = chunkSizeOverride ?? Math.Max(1, list.Count / Environment.ProcessorCount);
        var chunks = list.Chunk(chunkSize);

        var chunkTasks = chunks.Select(chunk => Task.Run(() =>
        {
            // Each chunk maps and folds independently
            return chunk.Select(mapper).Aggregate(_monoid.Identity, _monoid.Combine);
        }));

        var results = await Task.WhenAll(chunkTasks);
        return results.Aggregate(_monoid.Identity, _monoid.Combine);
    }

    // PLINQ with monoid safety
    public T PLinqMapFold<U>(IEnumerable<U> data, Func<U, T> mapper)
    {
        return data.AsParallel()
                  .Select(mapper)
                  .Aggregate(_monoid.Identity, _monoid.Combine);
    }
}

// Statistics monoid for parallel analysis
public struct Statistics
{
    public long Count { get; }
    public long Sum { get; }
    public long SumSquares { get; }

    public Statistics(long count, long sum, long sumSquares)
    {
        Count = count;
        Sum = sum;
        SumSquares = sumSquares;
    }

    public double Mean => Count > 0 ? (double)Sum / Count : 0;
    public double Variance => Count > 0 ? ((double)SumSquares / Count) - (Mean * Mean) : 0;
}

public class StatisticsMonoid : IMonoid<Statistics>
{
    public Statistics Identity => new Statistics(0, 0, 0);

    public Statistics Combine(Statistics a, Statistics b) =>
        new Statistics(a.Count + b.Count, a.Sum + b.Sum, a.SumSquares + b.SumSquares);
}

// Extension methods for easier usage
public static class MonoidExtensions
{
    public static async Task<T> ParallelFoldAsync<T>(this IEnumerable<T> source, IMonoid<T> monoid)
    {
        var processor = new MonoidParallelProcessor<T>(monoid);
        return await processor.ParallelFoldAsync(source);
    }

    public static IEnumerable<T[]> Chunk<T>(this IEnumerable<T> source, int chunkSize)
    {
        var enumerator = source.GetEnumerator();
        while (enumerator.MoveNext())
        {
            var chunk = new List<T> { enumerator.Current };
            for (int i = 1; i < chunkSize && enumerator.MoveNext(); i++)
            {
                chunk.Add(enumerator.Current);
            }
            yield return chunk.ToArray();
        }
    }
}

// Demonstration
class Program
{
    static async Task Main(string[] args)
    {
        var numbers = Enumerable.Range(1, 1_000_000).ToList();
        var emptyList = new List<int>();

        // Safe parallel sum - works with empty collections!
        var sumMonoid = new IntAddMonoid();
        var sumProcessor = new MonoidParallelProcessor<int>(sumMonoid);

        var parallelSum = await sumProcessor.ParallelMapFoldAsync(numbers, x => x);
        var emptySum = await sumProcessor.ParallelFoldAsync(emptyList);

        Console.WriteLine($"Parallel Sum: {parallelSum}");
        Console.WriteLine($"Empty List Sum: {emptySum}");  // 0, not null!

        // Parallel statistics collection
        var statsMonoid = new StatisticsMonoid();
        var statsProcessor = new MonoidParallelProcessor<Statistics>(statsMonoid);

        var stats = await statsProcessor.ParallelMapFoldAsync(
            numbers.Take(100_000),
            x => new Statistics(1, x, (long)x * x)
        );

        Console.WriteLine($"Count: {stats.Count}");
        Console.WriteLine($"Mean: {stats.Mean:F2}");
        Console.WriteLine($"Variance: {stats.Variance:F2}");

        // Empty statistics are safe too
        var emptyStats = await statsProcessor.ParallelFoldAsync(
            Enumerable.Empty<Statistics>()
        );
        Console.WriteLine($"Empty Stats: Count={emptyStats.Count}, Mean={emptyStats.Mean}");
    }
}
