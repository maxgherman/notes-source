using System;
using System.Linq;

// Domain types
public record Person(string Name, int Age, string Email);

// Maybe monad implementation
public interface IMaybe<T>
{
    IMaybe<U> FlatMap<U>(Func<T, IMaybe<U>> f);
    IMaybe<U> Map<U>(Func<T, U> f);
    T GetOrElse(T defaultValue);
}

public class Some<T> : IMaybe<T>
{
    private readonly T value;
    public Some(T value) { this.value = value; }

    public IMaybe<U> FlatMap<U>(Func<T, IMaybe<U>> f) => f(value);
    public IMaybe<U> Map<U>(Func<T, U> f) => new Some<U>(f(value));
    public T GetOrElse(T defaultValue) => value;
    public override string ToString() => $"Some({value})";
}

public class None<T> : IMaybe<T>
{
    public IMaybe<U> FlatMap<U>(Func<T, IMaybe<U>> f) => new None<U>();
    public IMaybe<U> Map<U>(Func<T, U> f) => new None<U>();
    public T GetOrElse(T defaultValue) => defaultValue;
    public override string ToString() => "None";
}

// Kleisli composition helper
public static class MaybeExtensions
{
    public static Func<A, IMaybe<C>> KleisliCompose<A, B, C>(
        Func<A, IMaybe<B>> f,
        Func<B, IMaybe<C>> g)
    {
        return a => f(a).FlatMap(g);
    }

    // LINQ support - required for query syntax
    public static IMaybe<U> Select<T, U>(this IMaybe<T> maybe, Func<T, U> selector)
    {
        return maybe.Map(selector);
    }

    public static IMaybe<U> SelectMany<T, U>(this IMaybe<T> maybe, Func<T, IMaybe<U>> selector)
    {
        return maybe.FlatMap(selector);
    }

    public static IMaybe<V> SelectMany<T, U, V>(
        this IMaybe<T> maybe,
        Func<T, IMaybe<U>> selector,
        Func<T, U, V> resultSelector)
    {
        return maybe.FlatMap(t => selector(t).Map(u => resultSelector(t, u)));
    }
}

public static class CSVTransformer
{
    // Kleisli arrows for CSV field transformations
    public static IMaybe<int> ParseAge(string ageStr)
    {
        return int.TryParse(ageStr, out int age)
            ? new Some<int>(age)
            : new None<int>();
    }

    public static IMaybe<int> ValidateAge(int age)
    {
        return (age >= 0 && age <= 150)
            ? new Some<int>(age)
            : new None<int>();
    }

    public static IMaybe<string> ParseEmail(string email)
    {
        return email.Contains("@")
            ? new Some<string>(email)
            : new None<string>();
    }

    // Composed Kleisli arrows
    public static readonly Func<string, IMaybe<int>> ProcessAge =
        MaybeExtensions.KleisliCompose<string, int, int>(ParseAge, ValidateAge);

    public static readonly Func<string, IMaybe<string>> ProcessEmail = ParseEmail;

    // CSV row transformation using Kleisli composition
    public static IMaybe<Person> TransformCSVRow(string[] row)
    {
        if (row.Length != 3) return new None<Person>();

        var (name, ageStr, emailStr) = (row[0], row[1], row[2]);

        return ProcessAge(ageStr)
            .FlatMap(validAge =>
                ProcessEmail(emailStr)
                    .Map(validEmail => new Person(name, validAge, validEmail))
            );
    }

    // Alternative using LINQ query syntax (which is Kleisli composition!)
    public static IMaybe<Person> TransformCSVRowLinq(string[] row)
    {
        if (row.Length != 3) return new None<Person>();

        var (name, ageStr, emailStr) = (row[0], row[1], row[2]);

        return
            from age in ProcessAge(ageStr)
            from email in ProcessEmail(emailStr)
            select new Person(name, age, email);
    }

    public static void Main()
    {
        var csvRows = new string[][]
        {
            new[] { "Alice", "25", "alice@example.com" },
            new[] { "Bob", "-5", "bob@example.com" },     // Invalid age
            new[] { "Charlie", "30", "invalid-email" },   // Invalid email
            new[] { "Diana", "28", "diana@example.com" }
        };

        var results = csvRows.Select(TransformCSVRow).ToArray();

        foreach (var result in results)
        {
            Console.WriteLine(result);
        }
        // Output:
        // Some(Person { Name = Alice, Age = 25, Email = alice@example.com })
        // None
        // None
        // Some(Person { Name = Diana, Age = 28, Email = diana@example.com })
    }
}
