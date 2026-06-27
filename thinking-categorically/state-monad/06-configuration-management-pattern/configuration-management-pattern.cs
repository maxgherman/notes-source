using System;
using System.Collections.Generic;
using System.Linq;

// Unit type for void-like operations
public struct Unit
{
    public static readonly Unit Default = new Unit();
}

public class State<S, A>
{
    private readonly Func<S, (A Result, S NewState)> computation;

    public State(Func<S, (A, S)> computation)
    {
        this.computation = computation;
    }

    public (A Result, S NewState) Run(S initialState)
    {
        return computation(initialState);
    }

    public State<S, B> Map<B>(Func<A, B> f)
    {
        return new State<S, B>(s =>
        {
            var (result, newState) = computation(s);
            return (f(result), newState);
        });
    }

    public State<S, B> FlatMap<B>(Func<A, State<S, B>> f)
    {
        return new State<S, B>(s =>
        {
            var (result, newState) = computation(s);
            return f(result).Run(newState);
        });
    }

    // LINQ Support
    public State<S, B> SelectMany<B>(Func<A, State<S, B>> f)
    {
        return FlatMap(f);
    }

    public State<S, C> SelectMany<B, C>(Func<A, State<S, B>> f, Func<A, B, C> projection)
    {
        return FlatMap(a => f(a).Map(b => projection(a, b)));
    }

    public State<S, B> Select<B>(Func<A, B> f)
    {
        return Map(f);
    }

    public static State<S, A> Pure(A value)
    {
        return new State<S, A>(s => (value, s));
    }

    public static State<S, S> Get()
    {
        return new State<S, S>(s => (s, s));
    }

    public static State<S, Unit> Put(S newState)
    {
        return new State<S, Unit>(_ => (Unit.Default, newState));
    }
}

// Application configuration
public class AppConfig
{
    public string ApiUrl { get; set; }
    public int Timeout { get; set; }
    public HashSet<string> Features { get; set; }

    public AppConfig(string apiUrl = "", int timeout = 0, HashSet<string>? features = null)
    {
        ApiUrl = apiUrl;
        Timeout = timeout;
        Features = features ?? new HashSet<string>();
    }

    public override string ToString()
    {
        return $"AppConfig(ApiUrl: \"{ApiUrl}\", Timeout: {Timeout}, Features: [{string.Join(", ", Features)}])";
    }
}

public static class Program
{
    // Enable a feature if not already enabled
    public static State<AppConfig, bool> EnableFeature(string feature)
    {
        return from config in State<AppConfig, AppConfig>.Get()
               let alreadyEnabled = config.Features.Contains(feature)
               from _ in alreadyEnabled ? State<AppConfig, Unit>.Put(config)
                                        : State<AppConfig, Unit>.Put(new AppConfig(config.ApiUrl, config.Timeout,
                                            new HashSet<string>(config.Features) { feature }))
               select !alreadyEnabled;
    }

    // Configure multiple features
    public static State<AppConfig, string> ConfigureApp()
    {
        return from darkMode in EnableFeature("darkMode")
               from notifications in EnableFeature("notifications")
               from analytics in EnableFeature("analytics")
               select $"Configured: darkMode={darkMode}, notifications={notifications}, analytics={analytics}";
    }

    // Usage example
    public static void Main()
    {
        var initialConfig = new AppConfig("https://api.example.com", 5000);
        var (result, finalConfig) = ConfigureApp().Run(initialConfig);

        Console.WriteLine(result);
        Console.WriteLine($"Final config: {finalConfig}");
        // Output shows which features were newly enabled
    }
}
