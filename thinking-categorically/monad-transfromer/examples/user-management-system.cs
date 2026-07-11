using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

// Domain types
public record User(int Id, string Name, string Email);

public record Config(string DbConnection, int MaxRetries, string LogLevel);

public abstract record AppError
{
    public record UserNotFound(int Id) : AppError;
    public record DatabaseError(string Message) : AppError;
    public record ValidationError(string Message) : AppError;
}

public record LogEntry(string Message);

// Result type for error handling
public interface IResult<T, E>
{
    IResult<U, E> Map<U>(Func<T, U> f);
    IResult<U, E> FlatMap<U>(Func<T, IResult<U, E>> f);
    IResult<T, F> MapError<F>(Func<E, F> f);
}

// Static factory class for Result
public static class Result
{
    public static IResult<T, E> Ok<T, E>(T value) => new Ok<T, E>(value);
    public static IResult<T, E> Error<T, E>(E error) => new Error<T, E>(error);
}

public class Ok<T, E> : IResult<T, E>
{
    public T Value { get; }

    public Ok(T value) { Value = value; }

    public IResult<U, E> Map<U>(Func<T, U> f) => new Ok<U, E>(f(Value));

    public IResult<U, E> FlatMap<U>(Func<T, IResult<U, E>> f) => f(Value);

    public IResult<T, F> MapError<F>(Func<E, F> f) => new Ok<T, F>(Value);

    public override string ToString() => $"Ok({Value})";
}

public class Error<T, E> : IResult<T, E>
{
    public E ErrorValue { get; }

    public Error(E error) { ErrorValue = error; }

    public IResult<U, E> Map<U>(Func<T, U> f) => new Error<U, E>(ErrorValue);

    public IResult<U, E> FlatMap<U>(Func<T, IResult<U, E>> f) => new Error<U, E>(ErrorValue);

    public IResult<T, F> MapError<F>(Func<E, F> f) => new Error<T, F>(f(ErrorValue));

    public override string ToString() => $"Error({ErrorValue})";
}

// Our monad transformer: ReaderT + ExceptT + WriterT + Task
public class AppM<T>
{
    private readonly Func<Config, Task<IResult<(T Value, List<LogEntry> Logs), AppError>>> computation;

    public AppM(Func<Config, Task<IResult<(T, List<LogEntry>), AppError>>> computation)
    {
        this.computation = computation;
    }

    // Run the computation
    public Task<IResult<(T Value, List<LogEntry> Logs), AppError>> Run(Config config)
    {
        return computation(config);
    }

    // Functor: map over the success value
    public AppM<U> Map<U>(Func<T, U> f)
    {
        return new AppM<U>(async config =>
        {
            var result = await computation(config);
            return result.Map(tuple => (f(tuple.Value), tuple.Logs));
        });
    }

    // Monad: flatMap for chaining operations
    public AppM<U> FlatMap<U>(Func<T, AppM<U>> f)
    {
        return new AppM<U>(async config =>
        {
            var result = await computation(config);

            if (result is Error<(T, List<LogEntry>), AppError> error)
                return new Error<(U, List<LogEntry>), AppError>(error.ErrorValue);

            var (value, logs1) = ((Ok<(T, List<LogEntry>), AppError>)result).Value;
            var nextResult = await f(value).computation(config);

            return nextResult.Map(tuple => (tuple.Value, logs1.Concat(tuple.Logs).ToList()));
        });
    }

    // Static constructors
    public static AppM<T> Pure(T value)
    {
        return new AppM<T>(_ => Task.FromResult(Result.Ok<(T, List<LogEntry>), AppError>((value, new List<LogEntry>()))));
    }

    public static AppM<Config> Ask()
    {
        return new AppM<Config>(config => Task.FromResult(Result.Ok<(Config, List<LogEntry>), AppError>((config, new List<LogEntry>()))));
    }

    public static AppM<Unit> Tell(LogEntry entry)
    {
        return new AppM<Unit>(_ => Task.FromResult(Result.Ok<(Unit, List<LogEntry>), AppError>((new Unit(), new List<LogEntry> { entry }))));
    }

    public static AppM<TResult> ThrowError<TResult>(AppError error)
    {
        return new AppM<TResult>(_ => Task.FromResult(Result.Error<(TResult, List<LogEntry>), AppError>(error)));
    }

    public static AppM<TResult> LiftIO<TResult>(Func<Task<TResult>> operation)
    {
        return new AppM<TResult>(async _ =>
        {
            try
            {
                var result = await operation();
                return Result.Ok<(TResult, List<LogEntry>), AppError>((result, new List<LogEntry>()));
            }
            catch (Exception ex)
            {
                return Result.Error<(TResult, List<LogEntry>), AppError>(new AppError.DatabaseError(ex.Message));
            }
        });
    }
}

public record Unit();

// LINQ support for AppM
public static class AppMExtensions
{
    public static AppM<TResult> Select<TSource, TResult>(this AppM<TSource> source, Func<TSource, TResult> selector)
    {
        return source.Map(selector);
    }

    public static AppM<TResult> SelectMany<TSource, TResult>(this AppM<TSource> source, Func<TSource, AppM<TResult>> selector)
    {
        return source.FlatMap(selector);
    }

    public static AppM<TFinal> SelectMany<TSource, TMiddle, TFinal>(this AppM<TSource> source, Func<TSource, AppM<TMiddle>> selector, Func<TSource, TMiddle, TFinal> resultSelector)
    {
        return source.FlatMap(t => selector(t).Map(u => resultSelector(t, u)));
    }
}

// Business operations
public static class UserOperations
{
    public static AppM<User> ValidateUser(User user)
    {
        return
            from _ in AppM<Unit>.Tell(new LogEntry($"Validating user: {user.Name}"))
            from config in AppM<Config>.Ask()
            from result in user.Name.Length < 3
                ? AppM<User>.ThrowError<User>(new AppError.ValidationError("Username too short"))
                : from __ in AppM<Unit>.Tell(new LogEntry("User validation passed"))
                  select user
            select result;
    }

    public static AppM<User> SaveUser(User user)
    {
        return
            from config in AppM<Config>.Ask()
            from _ in AppM<Unit>.Tell(new LogEntry($"Saving user to: {config.DbConnection}"))
            from __ in AppM<Unit>.LiftIO(async () =>
            {
                Console.WriteLine($"Connecting to: {config.DbConnection}");
                await Task.Delay(100); // Simulate async operation
                return new Unit();
            })
            from result in user.Id == 999
                ? AppM<User>.ThrowError<User>(new AppError.DatabaseError("Database connection failed"))
                : from ___ in AppM<Unit>.Tell(new LogEntry("User saved successfully"))
                  select user
            select result;
    }

    public static AppM<User> FindUser(int id)
    {
        return
            from _ in AppM<Unit>.Tell(new LogEntry($"Looking up user with ID: {id}"))
            from user in id == 1
                ? AppM<User>.Pure(new User(1, "alice", "alice@example.com"))
                : AppM<User>.ThrowError<User>(new AppError.UserNotFound(id))
            select user;
    }

    public static AppM<User> ProcessUser(int id, string name, string email)
    {
        return
            from _ in AppM<Unit>.Tell(new LogEntry("Starting user processing"))
            from validatedUser in ValidateUser(new User(id, name, email))
            from savedUser in SaveUser(validatedUser)
            from __ in AppM<Unit>.Tell(new LogEntry("User processing completed successfully"))
            select savedUser;
    }

    public static AppM<User> UpdateExistingUser(int id, string newName)
    {
        return
            from _ in AppM<Unit>.Tell(new LogEntry($"Updating user: {id}"))
            from existingUser in FindUser(id)
            from updatedUser in ValidateUser(existingUser with { Name = newName })
            from savedUser in SaveUser(updatedUser)
            select savedUser;
    }
}

// Example usage
public class Program
{
    public static async Task Main(string[] args)
    {
        var config = new Config("postgresql://localhost/mydb", 3, "INFO");

        Console.WriteLine("=== Creating new user ===");
        var result1 = await UserOperations.ProcessUser(42, "bob", "bob@example.com").Run(config);
        PrintResult(result1);

        Console.WriteLine("\n=== Updating existing user ===");
        var result2 = await UserOperations.UpdateExistingUser(1, "alice_updated").Run(config);
        PrintResult(result2);

        Console.WriteLine("\n=== Error case ===");
        var result3 = await UserOperations.ProcessUser(999, "baduser", "bad@example.com").Run(config);
        PrintResult(result3);
    }

    private static void PrintResult<T>(IResult<(T Value, List<LogEntry> Logs), AppError> result)
    {
        switch (result)
        {
            case Ok<(T, List<LogEntry>), AppError> ok:
                var (value, logs) = ok.Value;
                Console.WriteLine("Logs:");
                foreach (var log in logs)
                    Console.WriteLine($"  {log.Message}");
                Console.WriteLine($"Result: {value}");
                break;

            case Error<(T, List<LogEntry>), AppError> error:
                Console.WriteLine($"Error: {error.ErrorValue}");
                break;
        }
    }
}
