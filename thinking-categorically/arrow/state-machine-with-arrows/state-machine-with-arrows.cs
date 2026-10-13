// State machine Arrow
public class StateMachine<S, A, B>
{
    private readonly Func<S, A, (B, S)> run;
    public StateMachine(Func<S, A, (B, S)> run) { this.run = run; }
    public (B, S) Run(S state, A input) => run(state, input);

    // Identity
    public static StateMachine<S, A, A> Id() => new StateMachine<S, A, A>((s, a) => (a, s));

    // Composition
  public StateMachine<S, A, C> Then<C>(StateMachine<S, B, C> next) where C : notnull
    => new StateMachine<S, A, C>((s, a) => {
      var (b, s1) = this.Run(s, a);
      var (c, s2) = next.Run(s1, b);
      return (c, s2);
    });

    // Arrow combinator: arr
    public static StateMachine<S, A, B> Arr(Func<A, B> f)
        => new StateMachine<S, A, B>((s, a) => (f(a), s));

    // Arrow combinator: first
  public static StateMachine<S, (A, C), (B, C)> First<C>(StateMachine<S, A, B> sm) where C : notnull
    => new StateMachine<S, (A, C), (B, C)>((s, ac) => {
      var (a, c) = ac;
      var (b, s1) = sm.Run(s, a);
      return ((b, c), s1);
    });
}

// State machine primitives
public static class StateMachinePrimitives
{
    public static StateMachine<S, A, S> GetState<S, A>()
        => new StateMachine<S, A, S>((s, _) => (s, s));

    public static StateMachine<S, S, Unit> SetState<S>()
        => new StateMachine<S, S, Unit>((_, s) => (new Unit(), s));

    public static StateMachine<S, A, Unit> ModifyState<S, A>(Func<S, S> f)
        => new StateMachine<S, A, Unit>((s, _) => (new Unit(), f(s)));

    public struct Unit { }
}

// Traffic light state machine
public enum LightState { Red, Yellow, Green }
public enum LightInput { TimerTick, EmergencyStop }

public static class TrafficLightMachine
{
    public static LightState Transition(LightState state, LightInput input)
    {
        if (input == LightInput.EmergencyStop) return LightState.Red;
        if (state == LightState.Red && input == LightInput.TimerTick) return LightState.Green;
        if (state == LightState.Green && input == LightInput.TimerTick) return LightState.Yellow;
        if (state == LightState.Yellow && input == LightInput.TimerTick) return LightState.Red;
        return state;
    }

    public static StateMachine<LightState, LightInput, LightState> Machine =
        new StateMachine<LightState, LightInput, LightState>((state, input) => {
            var newState = Transition(state, input);
            return (newState, newState);
        });
}

// Counter with reset functionality
public static class CounterMachine
{
    public static StateMachine<int, string, int> Machine =
        new StateMachine<int, string, int>((state, command) => {
            int newState = state;
            if (command == "inc") newState++;
            else if (command == "dec") newState--;
            else if (command == "reset") newState = 0;
            return (newState, newState);
        });
}

// Example usage
public static class Program
{
    public static void Main()
    {
        // Counter
        Console.WriteLine("Counter:");
        int s = 0;
        var r = CounterMachine.Machine.Run(s, "inc"); Console.WriteLine(r); s = r.Item2;
        r = CounterMachine.Machine.Run(s, "dec"); Console.WriteLine(r); s = r.Item2;
        r = CounterMachine.Machine.Run(s, "reset"); Console.WriteLine(r);

        // Traffic light
        Console.WriteLine("\nTraffic Light:");
        LightState t = LightState.Red;
        var tl = TrafficLightMachine.Machine.Run(t, LightInput.TimerTick); Console.WriteLine(tl); t = tl.Item2;
        tl = TrafficLightMachine.Machine.Run(t, LightInput.TimerTick); Console.WriteLine(tl); t = tl.Item2;
        tl = TrafficLightMachine.Machine.Run(t, LightInput.EmergencyStop); Console.WriteLine(tl);
    }
}
