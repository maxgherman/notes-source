using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

// Unit type for void-like operations
public struct Unit
{
    public static readonly Unit Value = new Unit();
}

// Alternative-like pattern in C# using interface
public interface IOption<T>
{
    TResult Match<TResult>(
        Func<T, TResult> onSome,
        Func<TResult> onNone);
}

public class Some<T> : IOption<T>
{
    private readonly T _value;
    public Some(T value) => _value = value;

    public TResult Match<TResult>(
        Func<T, TResult> onSome,
        Func<TResult> onNone) => onSome(_value);
}

public class None<T> : IOption<T>
{
    public TResult Match<TResult>(
        Func<T, TResult> onSome,
        Func<TResult> onNone) => onNone();
}

// Static factory methods
public static class Option
{
    public static IOption<T> Some<T>(T value) => new Some<T>(value);
    public static IOption<T> None<T>() => new None<T>();
}

// Alternative operations
public static class OptionExtensions
{
    public static IOption<T> OrElse<T>(this IOption<T> first, IOption<T> second)
    {
        return first.Match(
            onSome: _ => first,
            onNone: () => second);
    }

    public static IOption<T> OrElse<T>(this IOption<T> first, Func<IOption<T>> secondFactory)
    {
        return first.Match(
            onSome: _ => first,
            onNone: secondFactory);
    }
}

// Configuration loading with multiple fallback strategies
public class ConfigurationLoader
{
    private readonly List<string> _searchPaths;

    public ConfigurationLoader()
    {
        _searchPaths = new List<string>
        {
            "./config.json",
            "./appsettings.json",
            "%APPDATA%/MyApp/config.json",
            "%PROGRAMDATA%/MyApp/config.json"
        };
    }

    // Try loading from file
    private IOption<string> LoadFromFile(string path)
    {
        try
        {
            var expandedPath = Environment.ExpandEnvironmentVariables(path);
            if (File.Exists(expandedPath))
            {
                var content = File.ReadAllText(expandedPath);
                return Option.Some(content);
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Failed to load config from {path}: {ex.Message}");
        }

        return Option.None<string>();
    }

    // Try loading from command line arguments
    private IOption<string> LoadFromArgs(string[] args)
    {
        var configArg = args
            .Where(arg => arg.StartsWith("--config="))
            .FirstOrDefault();

        if (configArg != null)
        {
            var path = configArg.Substring("--config=".Length);
            return LoadFromFile(path);
        }

        return Option.None<string>();
    }

    // Try loading from environment variable
    private IOption<string> LoadFromEnvironment()
    {
        var envPath = Environment.GetEnvironmentVariable("APP_CONFIG_PATH");
        return envPath != null ? LoadFromFile(envPath) : Option.None<string>();
    }

    // Try loading from multiple search paths
    private IOption<string> LoadFromSearchPaths()
    {
        return _searchPaths
            .Select(LoadFromFile)
            .Aggregate(Option.None<string>(), (acc, option) => acc.OrElse(option));
    }

    // Try loading default configuration
    private IOption<string> LoadDefault()
    {
        return Option.Some(@"{
            ""database"": {
                ""connectionString"": ""Data Source=localhost""
            },
            ""logging"": {
                ""level"": ""Info""
            }
        }");
    }

    // Main configuration loading with Alternative pattern
    public string LoadConfiguration(string[] args)
    {
        var config = LoadFromArgs(args)
            .OrElse(() => LoadFromEnvironment())
            .OrElse(() => LoadFromSearchPaths())
            .OrElse(() => LoadDefault());

        return config.Match(
            onSome: content => content,
            onNone: () => throw new InvalidOperationException("Could not load configuration")
        );
    }
}

// Connection string fallback example
public class DatabaseConnectionManager
{
    public IOption<string> GetConnectionString()
    {
        return TryProductionConnection()
            .OrElse(() => TryStagingConnection())
            .OrElse(() => TryDevelopmentConnection())
            .OrElse(() => TryLocalConnection());
    }

    private IOption<string> TryProductionConnection()
    {
        var connStr = Environment.GetEnvironmentVariable("PROD_DB_CONNECTION");
        return !string.IsNullOrEmpty(connStr) ?
            Option.Some(connStr) :
            Option.None<string>();
    }

    private IOption<string> TryStagingConnection()
    {
        var connStr = Environment.GetEnvironmentVariable("STAGING_DB_CONNECTION");
        return !string.IsNullOrEmpty(connStr) ?
            Option.Some(connStr) :
            Option.None<string>();
    }

    private IOption<string> TryDevelopmentConnection()
    {
        return Option.Some("Data Source=dev-server;Initial Catalog=DevDB");
    }

    private IOption<string> TryLocalConnection()
    {
        return Option.Some("Data Source=localhost;Initial Catalog=LocalDB");
    }
}

// Usage example
class Program
{
    static void Main(string[] args)
    {
        var configLoader = new ConfigurationLoader();
        try
        {
            var config = configLoader.LoadConfiguration(args);
            Console.WriteLine("Configuration loaded:");
            Console.WriteLine(config);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Failed to load configuration: {ex.Message}");
        }

        var dbManager = new DatabaseConnectionManager();
        var connectionString = dbManager.GetConnectionString();
        connectionString.Match<Unit>(
            onSome: conn => {
                Console.WriteLine($"Using connection: {conn}");
                return Unit.Value;
            },
            onNone: () => {
                Console.WriteLine("No database connection available");
                return Unit.Value;
            }
        );
    }
}
