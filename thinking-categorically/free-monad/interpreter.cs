using System;
using System.Collections.Generic;
using System.Threading.Tasks;

// Free Monad implementation using interfaces
public interface IFree<F, A>
{
    string Tag { get; }
    B Fold<B>(Func<A, B> pure, Func<F, B> free);
}

public interface IPure<F, A> : IFree<F, A>
{
    A Value { get; }
}

public interface IFreeBind<F, A> : IFree<F, A>
{
    F Fa { get; }
}

// Concrete implementations
public class Pure<F, A> : IPure<F, A>
{
    public string Tag => "Pure";
    public A Value { get; }

    public Pure(A value)
    {
        Value = value;
    }

    public B Fold<B>(Func<A, B> pure, Func<F, B> free)
    {
        return pure(Value);
    }
}

public class FreeBind<F, A> : IFreeBind<F, A>
{
    public string Tag => "FreeBind";
    public F Fa { get; }

    public FreeBind(F fa)
    {
        Fa = fa;
    }

    public B Fold<B>(Func<A, B> pure, Func<F, B> free)
    {
        return free(Fa);
    }
}

// Console operations using interfaces
public interface IConsoleF<A>
{
    string Tag { get; }
    IConsoleF<B> Map<B>(Func<A, B> f);
}

public interface IWriteLine<A> : IConsoleF<A>
{
    string Message { get; }
    A Next { get; }
}

public interface IReadLine<A> : IConsoleF<A>
{
    Func<string, A> Continuation { get; }
}

// Concrete console operations
public class WriteLine<A> : IWriteLine<A>
{
    public string Tag => "WriteLine";
    public string Message { get; }
    public A Next { get; }

    public WriteLine(string message, A next)
    {
        Message = message;
        Next = next;
    }

    public IConsoleF<B> Map<B>(Func<A, B> f)
    {
        return new WriteLine<B>(Message, f(Next));
    }
}

public class ReadLine<A> : IReadLine<A>
{
    public string Tag => "ReadLine";
    public Func<string, A> Continuation { get; }

    public ReadLine(Func<string, A> continuation)
    {
        Continuation = continuation;
    }

    public IConsoleF<B> Map<B>(Func<A, B> f)
    {
        return new ReadLine<B>(input => f(Continuation(input)));
    }
}

// Wrapper for our console Free Monad
public class ConsoleProgram<A> : IFree<IConsoleF<ConsoleProgram<A>>, A>
{
    private readonly IFree<IConsoleF<ConsoleProgram<A>>, A> _inner;

    public ConsoleProgram(IFree<IConsoleF<ConsoleProgram<A>>, A> inner)
    {
        _inner = inner;
    }

    public string Tag => _inner.Tag;

    public B Fold<B>(Func<A, B> pure, Func<IConsoleF<ConsoleProgram<A>>, B> free)
    {
        return _inner.Fold(pure, free);
    }
}

// Smart constructors
public static class Console
{
    public static ConsoleProgram<Unit> WriteLine(string message)
    {
        var writeOp = new WriteLine<ConsoleProgram<Unit>>(message, Pure(Unit.Instance));
        return new ConsoleProgram<Unit>(new FreeBind<IConsoleF<ConsoleProgram<Unit>>, Unit>(writeOp));
    }

    public static ConsoleProgram<string> ReadLine()
    {
        var readOp = new ReadLine<ConsoleProgram<string>>(input => Pure(input));
        return new ConsoleProgram<string>(new FreeBind<IConsoleF<ConsoleProgram<string>>, string>(readOp));
    }

    public static ConsoleProgram<A> Pure<A>(A value)
    {
        return new ConsoleProgram<A>(new Pure<IConsoleF<ConsoleProgram<A>>, A>(value));
    }

    // Monadic bind operation
    public static ConsoleProgram<B> Bind<A, B>(
        ConsoleProgram<A> ma,
        Func<A, ConsoleProgram<B>> f)
    {
        return new ConsoleProgram<B>(ma.Fold<IFree<IConsoleF<ConsoleProgram<B>>, B>>(
            a => f(a),
            fa => new FreeBind<IConsoleF<ConsoleProgram<B>>, B>(
                fa.Map<ConsoleProgram<B>>(nextProg => Bind(nextProg, f))
            )
        ));
    }
}

// Unit type for void operations
public class Unit
{
    public static readonly Unit Instance = new Unit();
    private Unit() { }
}

// Example programs
public static class Programs
{
    public static ConsoleProgram<Unit> GreetingProgram()
    {
        return Console.Bind(Console.WriteLine("Hello! What's your name?"), _ =>
            Console.Bind(Console.ReadLine(), name =>
                Console.Bind(Console.WriteLine($"Nice to meet you, {name}!"), _ =>
                    Console.Bind(Console.WriteLine("What's your favorite color?"), _ =>
                        Console.Bind(Console.ReadLine(), color =>
                            Console.WriteLine($"{name} likes {color}. Great choice!")
                        )
                    )
                )
            )
        );
    }

    public static ConsoleProgram<Unit> SurveyProgram()
    {
        return Console.Bind(Console.WriteLine("Welcome to our survey!"), _ =>
            Console.Bind(Console.WriteLine("Are you over 18? (yes/no)"), _ =>
                Console.Bind(Console.ReadLine(), age =>
                    age == "yes"
                        ? Console.Bind(Console.WriteLine("What's your occupation?"), _ =>
                            Console.Bind(Console.ReadLine(), job =>
                                Console.WriteLine($"Thank you! We have: occupation = {job}")
                            )
                        )
                        : Console.WriteLine("Thanks for your interest, but this survey is for adults only.")
                )
            )
        );
    }
}

// Real console interpreter
public static class ConsoleInterpreter
{
    public static async Task<A> RunConsoleIO<A>(ConsoleProgram<A> program)
    {
        return await Interpret(program);

        async Task<T> Interpret<T>(ConsoleProgram<T> prog)
        {
            return await prog.Fold<Task<T>>(
                value => Task.FromResult(value),
                async fa =>
                {
                    switch (fa.Tag)
                    {
                        case "WriteLine":
                            var writeOp = fa as IWriteLine<ConsoleProgram<T>>;
                            if (writeOp != null)
                            {
                                System.Console.WriteLine(writeOp.Message);
                                return await Interpret(writeOp.Next);
                            }
                            throw new InvalidOperationException("Invalid WriteLine operation");

                        case "ReadLine":
                            var readOp = fa as IReadLine<ConsoleProgram<T>>;
                            if (readOp != null)
                            {
                                var input = System.Console.ReadLine() ?? string.Empty;
                                return await Interpret(readOp.Continuation(input));
                            }
                            throw new InvalidOperationException("Invalid ReadLine operation");

                        default:
                            throw new InvalidOperationException("Unknown operation");
                    }
                }
            );
        }
    }
}

// Test interpreter (pure)
public class TestResult<A>
{
    public A? Result { get; set; }
    public List<string> Outputs { get; set; } = new List<string>();
    public List<string> InputsUsed { get; set; } = new List<string>();
}

public static class TestInterpreter
{
    public static TestResult<A> RunConsoleTest<A>(
        List<string> inputs,
        ConsoleProgram<A> program)
    {
        var inputIndex = 0;
        var outputs = new List<string>();
        var inputsUsed = new List<string>();

        T Interpret<T>(ConsoleProgram<T> prog)
        {
            return prog.Fold(
                value => value,
                fa =>
                {
                    switch (fa.Tag)
                    {
                        case "WriteLine":
                            var writeOp = fa as IWriteLine<ConsoleProgram<T>>;
                            if (writeOp != null)
                            {
                                outputs.Add(writeOp.Message);
                                return Interpret(writeOp.Next);
                            }
                            throw new InvalidOperationException("Invalid WriteLine operation");

                        case "ReadLine":
                            var readOp = fa as IReadLine<ConsoleProgram<T>>;
                            if (readOp != null)
                            {
                                if (inputIndex >= inputs.Count)
                                    throw new InvalidOperationException("No more test inputs available!");

                                var input = inputs[inputIndex++];
                                inputsUsed.Add(input);
                                return Interpret(readOp.Continuation(input));
                            }
                            throw new InvalidOperationException("Invalid ReadLine operation");

                        default:
                            throw new InvalidOperationException("Unknown operation");
                    }
                }
            );
        }

        var result = Interpret(program);
        return new TestResult<A>
        {
            Result = result,
            Outputs = outputs,
            InputsUsed = inputsUsed
        };
    }
}

// Mock interpreter
public class MockResult
{
    public List<string> Outputs { get; set; } = new List<string>();
    public List<string> InputsUsed { get; set; } = new List<string>();
}

public static class MockInterpreter
{
    public static MockResult RunConsoleMock<A>(
        List<string> inputs,
        ConsoleProgram<A> program)
    {
        var inputIndex = 0;
        var outputs = new List<string>();
        var inputsUsed = new List<string>();

        void Interpret<T>(ConsoleProgram<T> prog)
        {
            prog.Fold<Unit>(
                _ => Unit.Instance,
                fa =>
                {
                    switch (fa.Tag)
                    {
                        case "WriteLine":
                            var writeOp = fa as IWriteLine<ConsoleProgram<T>>;
                            if (writeOp != null)
                            {
                                outputs.Add(writeOp.Message);
                                Interpret(writeOp.Next);
                            }
                            break;

                        case "ReadLine":
                            var readOp = fa as IReadLine<ConsoleProgram<T>>;
                            if (readOp != null)
                            {
                                if (inputIndex >= inputs.Count)
                                    throw new InvalidOperationException("No mock input available!");

                                var input = inputs[inputIndex++];
                                inputsUsed.Add(input);
                                Interpret(readOp.Continuation(input));
                            }
                            break;

                        default:
                            throw new InvalidOperationException("Unknown operation");
                    }
                    return Unit.Instance;
                }
            );
        }

        Interpret(program);
        return new MockResult
        {
            Outputs = outputs,
            InputsUsed = inputsUsed
        };
    }
}

// Usage example
public class Program
{
    public static async Task Main(string[] args)
    {
        System.Console.WriteLine("=== Running with real IO ===");
        await ConsoleInterpreter.RunConsoleIO(Programs.GreetingProgram());

        System.Console.WriteLine("\n=== Running with test data ===");
        var testInputs = new List<string> { "Alice", "blue" };
        var testResult = TestInterpreter.RunConsoleTest(testInputs, Programs.GreetingProgram());
        System.Console.WriteLine("Outputs:");
        testResult.Outputs.ForEach(System.Console.WriteLine);
        System.Console.WriteLine("Inputs used:");
        testResult.InputsUsed.ForEach(System.Console.WriteLine);

        System.Console.WriteLine("\n=== Running with mock ===");
        var mockInputs = new List<string> { "Bob", "red" };
        var mockResult = MockInterpreter.RunConsoleMock(mockInputs, Programs.GreetingProgram());
        System.Console.WriteLine($"Mock outputs: [{string.Join(", ", mockResult.Outputs)}]");
        System.Console.WriteLine($"Mock inputs used: [{string.Join(", ", mockResult.InputsUsed)}]");
    }
}
