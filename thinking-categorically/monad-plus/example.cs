using System;
using System.Collections.Generic;
using System.Threading.Tasks;

// MonadPlus-like pattern in C#
public interface IAsyncOption<T>
{
    Task<TResult> MatchAsync<TResult>(
        Func<T, Task<TResult>> onSuccess,
        Func<Task<TResult>> onFailure);
}

public class AsyncSuccess<T> : IAsyncOption<T>
{
    private readonly T _value;
    public AsyncSuccess(T value) => _value = value;

    public async Task<TResult> MatchAsync<TResult>(
        Func<T, Task<TResult>> onSuccess,
        Func<Task<TResult>> onFailure)
    {
        return await onSuccess(_value);
    }
}

public class AsyncFailure<T> : IAsyncOption<T>
{
    private readonly Exception error;
    public AsyncFailure(Exception error) => this.error = error;

    public async Task<TResult> MatchAsync<TResult>(
        Func<T, Task<TResult>> onSuccess,
        Func<Task<TResult>> onFailure)
    {
        return await onFailure();
    }
}

// MonadPlus operations
public static class AsyncOption
{
    public static IAsyncOption<T> Success<T>(T value) => new AsyncSuccess<T>(value);
    public static IAsyncOption<T> Failure<T>(Exception error) => new AsyncFailure<T>(error);
    public static IAsyncOption<T> MZero<T>() => new AsyncFailure<T>(new InvalidOperationException("MZero"));
}

public static class AsyncOptionExtensions
{
    // Monadic bind
    public static async Task<IAsyncOption<TResult>> FlatMapAsync<T, TResult>(
        this IAsyncOption<T> option,
        Func<T, Task<IAsyncOption<TResult>>> selector)
    {
        return await option.MatchAsync(
            onSuccess: selector,
            onFailure: () => Task.FromResult(AsyncOption.Failure<TResult>(new InvalidOperationException("Previous operation failed")))
        );
    }

    // MonadPlus choice (mplus equivalent)
    public static IAsyncOption<T> OrElse<T>(this IAsyncOption<T> first, Func<Task<IAsyncOption<T>>> second)
    {
        return new AsyncChoice<T>(first, second);
    }
}

// Lazy evaluation for choice
public class AsyncChoice<T> : IAsyncOption<T>
{
    private readonly IAsyncOption<T> _first;
    private readonly Func<Task<IAsyncOption<T>>> _second;

    public AsyncChoice(IAsyncOption<T> first, Func<Task<IAsyncOption<T>>> second)
    {
        _first = first;
        _second = second;
    }

    public async Task<TResult> MatchAsync<TResult>(
        Func<T, Task<TResult>> onSuccess,
        Func<Task<TResult>> onFailure)
    {
        var firstResult = await _first.MatchAsync(
            onSuccess: async value => (true, await onSuccess(value)),
            onFailure: () => Task.FromResult((false, default(TResult)!))
        );

        if (firstResult.Item1)
        {
            return firstResult.Item2;
        }

        var secondOption = await _second();
        return await secondOption.MatchAsync(onSuccess, onFailure);
    }
}

// Database entities
public class Customer
{
    public int Id { get; set; }
    public string Name { get; set; } = string.Empty;
    public string Email { get; set; } = string.Empty;
    public DateTime CreatedAt { get; set; }
}

// Database connection interfaces
public interface IDatabase
{
    Task<IAsyncOption<Customer>> GetCustomerAsync(int id);
    Task<IAsyncOption<List<Customer>>> GetCustomersAsync(string namePrefix);
}

// Primary database connection
public class PrimaryDatabase : IDatabase
{
    public async Task<IAsyncOption<Customer>> GetCustomerAsync(int id)
    {
        try
        {
            await Task.Delay(100); // Simulate network delay

            // Simulate occasional failures
            if (new Random().NextDouble() > 0.6)
            {
                return AsyncOption.Success(new Customer
                {
                    Id = id,
                    Name = $"Customer {id}",
                    Email = $"customer{id}@primary.db",
                    CreatedAt = DateTime.Now
                });
            }

            return AsyncOption.Failure<Customer>(new TimeoutException("Primary database timeout"));
        }
        catch (Exception ex)
        {
            return AsyncOption.Failure<Customer>(ex);
        }
    }

    public async Task<IAsyncOption<List<Customer>>> GetCustomersAsync(string namePrefix)
    {
        try
        {
            await Task.Delay(150);

            if (new Random().NextDouble() > 0.5)
            {
                var customers = new List<Customer>();
                for (int i = 1; i <= 3; i++)
                {
                    customers.Add(new Customer
                    {
                        Id = i,
                        Name = $"{namePrefix}Customer{i}",
                        Email = $"{namePrefix.ToLower()}{i}@primary.db",
                        CreatedAt = DateTime.Now
                    });
                }
                return AsyncOption.Success(customers);
            }

            return AsyncOption.Failure<List<Customer>>(new InvalidOperationException("Primary database overloaded"));
        }
        catch (Exception ex)
        {
            return AsyncOption.Failure<List<Customer>>(ex);
        }
    }
}

// Secondary database connection
public class SecondaryDatabase : IDatabase
{
    public async Task<IAsyncOption<Customer>> GetCustomerAsync(int id)
    {
        try
        {
            await Task.Delay(200);

            if (new Random().NextDouble() > 0.3)
            {
                return AsyncOption.Success(new Customer
                {
                    Id = id,
                    Name = $"SecondaryCustomer {id}",
                    Email = $"customer{id}@secondary.db",
                    CreatedAt = DateTime.Now.AddDays(-1)
                });
            }

            return AsyncOption.Failure<Customer>(new InvalidOperationException("Secondary database unavailable"));
        }
        catch (Exception ex)
        {
            return AsyncOption.Failure<Customer>(ex);
        }
    }

    public async Task<IAsyncOption<List<Customer>>> GetCustomersAsync(string namePrefix)
    {
        try
        {
            await Task.Delay(250);

            var customers = new List<Customer>
            {
                new Customer { Id = 10, Name = $"{namePrefix}SecondaryCustomer", Email = $"{namePrefix.ToLower()}@secondary.db", CreatedAt = DateTime.Now.AddDays(-1) }
            };
            return AsyncOption.Success(customers);
        }
        catch (Exception ex)
        {
            return AsyncOption.Failure<List<Customer>>(ex);
        }
    }
}

// Cache layer
public class CacheDatabase : IDatabase
{
    private readonly Dictionary<int, Customer> _cache = new Dictionary<int, Customer>
    {
        { 1, new Customer { Id = 1, Name = "CachedCustomer 1", Email = "cached1@cache.local", CreatedAt = DateTime.Now.AddMinutes(-5) } },
        { 2, new Customer { Id = 2, Name = "CachedCustomer 2", Email = "cached2@cache.local", CreatedAt = DateTime.Now.AddMinutes(-3) } }
    };

    public async Task<IAsyncOption<Customer>> GetCustomerAsync(int id)
    {
        await Task.Delay(10); // Simulate cache lookup time

        if (_cache.TryGetValue(id, out var customer))
        {
            return AsyncOption.Success(customer);
        }

        return AsyncOption.Failure<Customer>(new KeyNotFoundException($"Customer {id} not found in cache"));
    }

    public async Task<IAsyncOption<List<Customer>>> GetCustomersAsync(string namePrefix)
    {
        await Task.Delay(5);

        var customers = new List<Customer>(_cache.Values);
        return AsyncOption.Success(customers);
    }
}

// Service using MonadPlus pattern
public class CustomerService
{
    private readonly IDatabase primary;
    private readonly IDatabase secondary;
    private readonly IDatabase cache;

    public CustomerService(
        IDatabase primary,
        IDatabase secondary,
        IDatabase cache)
    {
        this.primary = primary;
        this.secondary = secondary;
        this.cache = cache;
    }

    // MonadPlus: Try multiple data sources with fallback
    public async Task<Customer> GetCustomerAsync(int id)
    {
        var primaryResult = await primary.GetCustomerAsync(id);
        var result = primaryResult
            .OrElse(() => secondary.GetCustomerAsync(id))
            .OrElse(() => cache.GetCustomerAsync(id));

        return await result.MatchAsync(
            onSuccess: customer => Task.FromResult(customer),
            onFailure: () => throw new InvalidOperationException($"Customer {id} not found in any data source")
        );
    }

    // Monadic composition: Chain dependent operations
    public async Task<string> GetCustomerReportAsync(int id)
    {
        var primaryCustomer = await primary.GetCustomerAsync(id);
        var customerOption = primaryCustomer
            .OrElse(() => secondary.GetCustomerAsync(id))
            .OrElse(() => cache.GetCustomerAsync(id));

        var reportOption = await customerOption.FlatMapAsync(async customer =>
        {
            // Simulate report generation
            await Task.Delay(100);
            var report = $"Customer Report:\nID: {customer.Id}\nName: {customer.Name}\nEmail: {customer.Email}\nCreated: {customer.CreatedAt:yyyy-MM-dd}";
            return AsyncOption.Success(report);
        });

        return await reportOption.MatchAsync(
            onSuccess: report => Task.FromResult(report),
            onFailure: () => throw new InvalidOperationException($"Could not generate report for customer {id}")
        );
    }
}

// Usage example
class Program
{
    static async Task Main(string[] args)
    {
        var customerService = new CustomerService(
            new PrimaryDatabase(),
            new SecondaryDatabase(),
            new CacheDatabase()
        );

        Console.WriteLine("Testing MonadPlus database fallback:");

        for (int i = 1; i <= 5; i++)
        {
            try
            {
                var customer = await customerService.GetCustomerAsync(i);
                Console.WriteLine($"Customer {i}: {customer.Name} ({customer.Email})");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Failed to get customer {i}: {ex.Message}");
            }
        }

        Console.WriteLine("\nTesting monadic composition (reports):");

        try
        {
            var report = await customerService.GetCustomerReportAsync(1);
            Console.WriteLine(report);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Failed to generate report: {ex.Message}");
        }
    }
}
