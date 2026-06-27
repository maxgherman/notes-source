using System;
using System.Collections.Generic;
using System.Linq;

// State monad implementation
public class State<S, A>
{
    private readonly Func<S, (A Result, S NewState)> computation;

    public State(Func<S, (A, S)> computation)
    {
        this.computation = computation;
    }

    // Run the state computation
    public (A Result, S NewState) Run(S initialState)
    {
        return computation(initialState);
    }

    // Extract just the result
    public A Eval(S initialState)
    {
        return Run(initialState).Result;
    }

    // Extract just the final state
    public S Exec(S initialState)
    {
        return Run(initialState).NewState;
    }

    // Functor: map over the result
    public State<S, B> Map<B>(Func<A, B> f)
    {
        return new State<S, B>(s =>
        {
            var (result, newState) = computation(s);
            return (f(result), newState);
        });
    }

    // Monad: bind/flatMap
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

    // Static constructors
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

    public static State<S, Unit> Modify(Func<S, S> f)
    {
        return new State<S, Unit>(s => (Unit.Default, f(s)));
    }
}

// Unit type for void-like operations
public struct Unit
{
    public static readonly Unit Default = new Unit();
}

// Game state class
public class GameState
{
    public int Health { get; set; }
    public int Score { get; set; }
    public List<string> Inventory { get; set; }
    public (int X, int Y) Position { get; set; }

    public GameState()
    {
        Health = 100;
        Score = 0;
        Inventory = new List<string>();
        Position = (0, 0);
    }

    public GameState(GameState other)
    {
        Health = other.Health;
        Score = other.Score;
        Inventory = new List<string>(other.Inventory);
        Position = other.Position;
    }

    public override string ToString()
    {
        return $"GameState {{ Health = {Health}, Score = {Score}, " +
               $"Inventory = [{string.Join(", ", Inventory)}], Position = {Position} }}";
    }
}

public static class GameOperations
{
    // Game operations using State monad
    public static State<GameState, string> CollectItem(string item)
    {
        return from state in State<GameState, GameState>.Get()
               from _ in State<GameState, Unit>.Put(new GameState(state)
               {
                   Inventory = state.Inventory.Concat(new[] { item }).ToList(),
                   Score = state.Score + 10
               })
               select $"Collected {item}";
    }

    public static State<GameState, string> TakeDamage(int damage)
    {
        return from state in State<GameState, GameState>.Get()
               let newHealth = Math.Max(0, state.Health - damage)
               from _ in State<GameState, Unit>.Put(new GameState(state) { Health = newHealth })
               select newHealth > 0 ? "Still alive" : "Game over";
    }

    public static State<GameState, string> MovePlayer(int dx, int dy)
    {
        return from state in State<GameState, GameState>.Get()
               let newPos = (state.Position.X + dx, state.Position.Y + dy)
               from _ in State<GameState, Unit>.Put(new GameState(state) { Position = newPos })
               select $"Moved to {newPos}";
    }

    // Monadic game sequence using LINQ syntax
    public static State<GameState, List<string>> PlayGame()
    {
        return from msg1 in CollectItem("sword")
               from msg2 in TakeDamage(20)
               from msg3 in MovePlayer(5, 3)
               select new List<string> { msg1, msg2, msg3 };
    }
}

class Program
{
    static void Main()
    {
        var initialState = new GameState();
        var (messages, finalState) = GameOperations.PlayGame().Run(initialState);

        Console.WriteLine("Game Messages:");
        foreach (var message in messages)
        {
            Console.WriteLine($"  {message}");
        }

        Console.WriteLine($"\nFinal State: {finalState}");
    }
}
