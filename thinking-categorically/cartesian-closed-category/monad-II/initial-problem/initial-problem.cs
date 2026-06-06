using System;
using System.Collections.Generic;
using System.Linq;

// Configuration
public class Config
{
    public int AgeThreshold { get; set; }
    public string Locale { get; set; } = "en";
    public bool RequireValidAge { get; set; }
}

// Transformation errors
public interface ITransformationError
{
    string Message { get; }
}

public class InvalidAgeError : ITransformationError
{
    public string Value { get; }
    public InvalidAgeError(string value) => Value = value;
    public string Message => $"Invalid age: {Value}";
}

public class MissingFieldError : ITransformationError
{
    public string Field { get; }
    public MissingFieldError(string field) => Field = field;
    public string Message => $"Missing field: {Field}";
}

public class AgeOutOfRangeError : ITransformationError
{
    public int Age { get; }
    public AgeOutOfRangeError(int age) => Age = age;
    public string Message => $"Age out of range: {Age}";
}

// Result monad
public interface IResult<T, E>
{
    bool IsOk { get; }
    bool IsError { get; }
    IResult<U, E> Bind<U>(Func<T, IResult<U, E>> f);
    IResult<U, E> Map<U>(Func<T, U> f);
}

public static class Result<T, E>
{
    public static IResult<T, E> Ok(T value) => new OkResult<T, E>(value);
    public static IResult<T, E> Error(E error) => new ErrorResult<T, E>(error);
}

public class OkResult<T, E> : IResult<T, E>
{
    public T Value { get; }
    public OkResult(T value) => Value = value;

    public bool IsOk => true;
    public bool IsError => false;

    public IResult<U, E> Bind<U>(Func<T, IResult<U, E>> f) => f(Value);
    public IResult<U, E> Map<U>(Func<T, U> f) => Result<U, E>.Ok(f(Value));
}

public class ErrorResult<T, E> : IResult<T, E>
{
    public E Error { get; }
    public ErrorResult(E error) => Error = error;

    public bool IsOk => false;
    public bool IsError => true;

    public IResult<U, E> Bind<U>(Func<T, IResult<U, E>> f) =>
        new ErrorResult<U, E>(Error);
    public IResult<U, E> Map<U>(Func<T, U> f) =>
        new ErrorResult<U, E>(Error);
}

// Reader monad with Result
public class ReaderResult<R, T, E>
{
    private readonly Func<R, IResult<T, E>> computation;

    public ReaderResult(Func<R, IResult<T, E>> computation)
    {
        this.computation = computation;
    }

    public static ReaderResult<R, T, E> Pure(T value) =>
        new ReaderResult<R, T, E>(_ => Result<T, E>.Ok(value));

    public static ReaderResult<R, T, E> Error(E error) =>
        new ReaderResult<R, T, E>(_ => Result<T, E>.Error(error));

    public ReaderResult<R, U, E> Bind<U>(Func<T, ReaderResult<R, U, E>> f) =>
        new ReaderResult<R, U, E>(config =>
        {
            var result = computation(config);
            return result.IsOk
                ? f(((OkResult<T, E>)result).Value).Run(config)
                : Result<U, E>.Error(((ErrorResult<T, E>)result).Error);
        });

    public ReaderResult<R, U, E> Map<U>(Func<T, U> f) =>
        new ReaderResult<R, U, E>(config => computation(config).Map(f));

    public IResult<T, E> Run(R config) => computation(config);

    // LINQ support
    public ReaderResult<R, U, E> SelectMany<U>(Func<T, ReaderResult<R, U, E>> f) => Bind(f);

    public ReaderResult<R, V, E> SelectMany<U, V>(
        Func<T, ReaderResult<R, U, E>> f,
        Func<T, U, V> projection) =>
        Bind(t => f(t).Map(u => projection(t, u)));
}

// Person result type
public class Person
{
    public string Name { get; set; } = "";
    public int Age { get; set; }
    public string AgeGroup { get; set; } = "";
    public List<string> TransformationHistory { get; set; } = new();

    public override string ToString() =>
        $"Person {{ Name = \"{Name}\", Age = {Age}, AgeGroup = \"{AgeGroup}\", " +
        $"History = [{string.Join(", ", TransformationHistory.Select(h => $"\"{h}\""))}] }}";
}

public class CSVTransformer
{
    // Parse age with monadic error handling
    static ReaderResult<Config, int, ITransformationError> ParseAge(string ageStr) =>
        new ReaderResult<Config, int, ITransformationError>(config =>
        {
            if (!int.TryParse(ageStr, out var age))
                return Result<int, ITransformationError>.Error(new InvalidAgeError(ageStr));

            if (config.RequireValidAge && (age < 0 || age > 150))
                return Result<int, ITransformationError>.Error(new AgeOutOfRangeError(age));

            return Result<int, ITransformationError>.Ok(age);
        });

    // Categorize age based on configuration
    static ReaderResult<Config, string, ITransformationError> CategorizeAge(int age) =>
        new ReaderResult<Config, string, ITransformationError>(config =>
        {
            var ageGroup = age > config.AgeThreshold ? "old" : "young";
            return Result<string, ITransformationError>.Ok(ageGroup);
        });

    // Localize name based on configuration
    static ReaderResult<Config, string, ITransformationError> LocalizeName(string name) =>
        new ReaderResult<Config, string, ITransformationError>(config =>
        {
            var localizedName = config.Locale == "de"
                ? name.ToUpperInvariant()
                : name.ToLowerInvariant();
            return Result<string, ITransformationError>.Ok(localizedName);
        });

    // Monadic transformation pipeline using LINQ syntax
    public static ReaderResult<Config, Person, ITransformationError> TransformRow(Dictionary<string, string> row)
    {
        if (!row.ContainsKey("name") || string.IsNullOrEmpty(row["name"]))
            return ReaderResult<Config, Person, ITransformationError>.Error(new MissingFieldError("name"));

        if (!row.ContainsKey("age") || string.IsNullOrEmpty(row["age"]))
            return ReaderResult<Config, Person, ITransformationError>.Error(new MissingFieldError("age"));

        var name = row["name"];
        var ageStr = row["age"];

        // Sequential monadic composition using LINQ
        return from age in ParseAge(ageStr)
               from ageGroup in CategorizeAge(age)
               from localizedName in LocalizeName(name)
               select new Person
               {
                   Name = localizedName,
                   Age = age,
                   AgeGroup = ageGroup,
                   TransformationHistory = new List<string>
                   {
                       $"Parsed age: {age}",
                       $"Categorized as: {ageGroup}",
                       $"Localized name: {localizedName}"
                   }
               };
    }

    // Process entire CSV with error collection
    public static IResult<List<Person>, List<ITransformationError>> TransformCSVData(
        List<Dictionary<string, string>> csvData,
        Config config)
    {
        var results = new List<Person>();
        var errors = new List<ITransformationError>();

        foreach (var row in csvData)
        {
            var result = TransformRow(row).Run(config);

            if (result.IsOk)
                results.Add(((OkResult<Person, ITransformationError>)result).Value);
            else
                errors.Add(((ErrorResult<Person, ITransformationError>)result).Error);
        }

        return errors.Any()
            ? Result<List<Person>, List<ITransformationError>>.Error(errors)
            : Result<List<Person>, List<ITransformationError>>.Ok(results);
    }

    // Simple CSV parser
    public static List<Dictionary<string, string>> ReadCSV(string csv)
    {
        var lines = csv.Trim().Split(new[] { '\n', '\r' }, StringSplitOptions.RemoveEmptyEntries);
        if (lines.Length < 2) return new List<Dictionary<string, string>>();

        var headers = lines[0].Split(',').Select(h => h.Trim()).ToArray();
        return lines.Skip(1).Select(line =>
        {
            var values = line.Split(',').Select(v => v.Trim()).ToArray();
            return headers.Zip(values, (k, v) => new { k, v })
                          .ToDictionary(x => x.k, x => x.v);
        }).ToList();
    }
}

class Program
{
    static void Main()
    {
        // var csvData = CSVTransformer.ReadCSV("name,age\nAlice,25\nBob,31\nCharlie,35");
        var csvData = CSVTransformer.ReadCSV("name,age\nAlice,25\nBob,invalid\nCharlie,35");
        var config = new Config
        {
            AgeThreshold = 30,
            Locale = "de",
            RequireValidAge = true
        };

        var result = CSVTransformer.TransformCSVData(csvData, config);

        if (result.IsOk)
        {
            Console.WriteLine("Transformed data:");
            foreach (var person in ((OkResult<List<Person>, List<ITransformationError>>)result).Value)
                Console.WriteLine(person);
        }
        else
        {
            Console.WriteLine("Transformation errors:");
            foreach (var error in ((ErrorResult<List<Person>, List<ITransformationError>>)result).Error)
                Console.WriteLine(error.Message);
        }
    }
}
