using System;
using System.Collections.Generic;
using System.Linq;

// Parser interface mimicking Arrow behavior for parsing
public interface IParser<TInput, TOutput>
    where TOutput : class
{
    TOutput? Parse(TInput input);
}

// Concrete parser implementation
public class Parser<TInput, TOutput> : IParser<TInput, TOutput>
    where TOutput : class
{
    private readonly Func<TInput, TOutput?> _parseFunc;

    public Parser(Func<TInput, TOutput?> parseFunc)
    {
        _parseFunc = parseFunc;
    }

    public TOutput? Parse(TInput input) => _parseFunc(input);
}

// Static factory and composition methods
public static class Parser
{
    // Helper to create parsers (similar to 'arr')
    public static IParser<TInput, TOutput> From<TInput, TOutput>(
        Func<TInput, TOutput?> parseFunc)
        where TOutput : class
    {
        return new Parser<TInput, TOutput>(parseFunc);
    }

    // Parser composition operator (similar to '>>>')
    public static IParser<TInput, TOutput3> Compose<TInput, TOutput1, TOutput3>(
        IParser<TInput, TOutput1> first,
        IParser<TOutput1, TOutput3> second)
        where TOutput1 : class
        where TOutput3 : class
    {
        return From<TInput, TOutput3>(input =>
        {
            var intermediate = first.Parse(input);
            return intermediate != null ? second.Parse(intermediate) : default;
        });
    }

    // Tuple wrapper class to satisfy reference type constraint
    public class ParsedPair<T1, T2>
        where T1 : class
        where T2 : class
    {
        public T1 First { get; }
        public T2 Second { get; }

        public ParsedPair(T1 first, T2 second)
        {
            First = first;
            Second = second;
        }

        public void Deconstruct(out T1 first, out T2 second)
        {
            first = First;
            second = Second;
        }
    }

    // Parallel parser composition for tuples
    public static IParser<(TInput1, TInput2), ParsedPair<TOutput1, TOutput2>> Parallel<TInput1, TInput2, TOutput1, TOutput2>(
        IParser<TInput1, TOutput1> left,
        IParser<TInput2, TOutput2> right)
        where TOutput1 : class
        where TOutput2 : class
    {
        return From<(TInput1, TInput2), ParsedPair<TOutput1, TOutput2>>(inputs =>
        {
            var leftResult = left.Parse(inputs.Item1);
            var rightResult = right.Parse(inputs.Item2);
            return leftResult != null && rightResult != null
                ? new ParsedPair<TOutput1, TOutput2>(leftResult, rightResult)
                : default;
        });
    }

    // Alternative combinator (try first, then second)
    public static IParser<TInput, TOutput> Alternative<TInput, TOutput>(
        IParser<TInput, TOutput> first,
        IParser<TInput, TOutput> second)
        where TOutput : class
    {
        return From<TInput, TOutput>(input =>
        {
            var firstResult = first.Parse(input);
            return firstResult ?? second.Parse(input);
        });
    }
}

// Extension methods for fluent interface
public static class ParserExtensions
{
    public static IParser<TInput, TOutput3> Then<TInput, TOutput1, TOutput3>(
        this IParser<TInput, TOutput1> first,
        IParser<TOutput1, TOutput3> second)
        where TOutput1 : class
        where TOutput3 : class
    {
        return Parser.Compose(first, second);
    }

    public static IParser<TInput, TResult> Map<TInput, TOutput, TResult>(
        this IParser<TInput, TOutput> parser,
        Func<TOutput, TResult> mapper)
        where TOutput : class
        where TResult : class
    {
        return Parser.From<TInput, TResult>(input =>
        {
            var result = parser.Parse(input);
            return result != null ? mapper(result) : default;
        });
    }

    public static IParser<TInput, TOutput> Or<TInput, TOutput>(
        this IParser<TInput, TOutput> first,
        IParser<TInput, TOutput> second)
        where TOutput : class
    {
        return Parser.Alternative(first, second);
    }
}

// Basic parsing primitives
public static class BasicParsers
{
    public static IParser<string, string> CharParser(char target) =>
        Parser.From<string, string>(input =>
            input.Length > 0 && input[0] == target ? target.ToString() : default);

    public static IParser<string, string> DigitParser =>
        Parser.From<string, string>(input =>
        {
            if (input.Length == 0) return default;
            var c = input[0];
            return char.IsDigit(c) ? (c - '0').ToString() : default;
        });

    public static IParser<string, string> ParseFirstDigit =>
        Parser.From<string, string>(input =>
        {
            if (input.Length == 0) return default;
            var firstChar = input[0];
            return char.IsDigit(firstChar) ? (firstChar - '0').ToString() : default;
        });
}

// Stateful parsing for position tracking
public struct ParseState
{
    public string Input { get; }
    public int Position { get; }

    public ParseState(string input, int position = 0)
    {
        Input = input;
        Position = position;
    }
}

public struct ParseResult<T>
{
    public T Result { get; }
    public ParseState NewState { get; }

    public ParseResult(T result, ParseState newState)
    {
        Result = result;
        NewState = newState;
    }
}

// Stateful parser interface
public interface IStatefulParser<T>
{
    ParseResult<T>? Parse(ParseState state);
}

public class StatefulParser<T> : IStatefulParser<T>
{
    private readonly Func<ParseState, ParseResult<T>?> _parseFunc;

    public StatefulParser(Func<ParseState, ParseResult<T>?> parseFunc)
    {
        _parseFunc = parseFunc;
    }

    public ParseResult<T>? Parse(ParseState state) => _parseFunc(state);
}

// Stateful parser factory and combinators
public static class StatefulParser
{
    // Wrapper classes for value types to satisfy reference type constraints
    public class IntValue
    {
        public int Value { get; }
        public IntValue(int value) => Value = value;
        public static implicit operator int(IntValue wrapper) => wrapper.Value;
        public static implicit operator IntValue(int value) => new IntValue(value);
        public override string ToString() => Value.ToString();
    }

    public class CharValue
    {
        public char Value { get; }
        public CharValue(char value) => Value = value;
        public static implicit operator char(CharValue wrapper) => wrapper.Value;
        public static implicit operator CharValue(char value) => new CharValue(value);
        public override string ToString() => Value.ToString();
    }

    public static IStatefulParser<T> From<T>(Func<ParseState, ParseResult<T>?> parseFunc) =>
        new StatefulParser<T>(parseFunc);

    public static IStatefulParser<CharValue> CharParser(char target) =>
        From<CharValue>(state =>
        {
            if (state.Position >= state.Input.Length) return null;
            var currentChar = state.Input[state.Position];
            if (currentChar != target) return null;

            return new ParseResult<CharValue>(
                new CharValue(currentChar),
                new ParseState(state.Input, state.Position + 1)
            );
        });

    public static IStatefulParser<IntValue> DigitParser =>
        From<IntValue>(state =>
        {
            if (state.Position >= state.Input.Length) return null;
            var currentChar = state.Input[state.Position];
            if (!char.IsDigit(currentChar)) return null;

            return new ParseResult<IntValue>(
                new IntValue(currentChar - '0'),
                new ParseState(state.Input, state.Position + 1)
            );
        });

    public static IStatefulParser<IntValue> ParseNumber =>
        From<IntValue>(state =>
        {
            var currentState = state;
            var digits = new List<int>();

            // Parse first digit (required)
            var firstResult = DigitParser.Parse(currentState);
            if (firstResult == null) return null;

            digits.Add(firstResult.Value.Result.Value);
            currentState = firstResult.Value.NewState;

            // Parse additional digits (optional)
            while (currentState.Position < currentState.Input.Length)
            {
                var digitResult = DigitParser.Parse(currentState);
                if (digitResult == null) break;

                digits.Add(digitResult.Value.Result.Value);
                currentState = digitResult.Value.NewState;
            }

            // Convert digits to number
            var result = digits.Aggregate(0, (acc, digit) => acc * 10 + digit);
            return new ParseResult<IntValue>(new IntValue(result), currentState);
        });

    public static IStatefulParser<string> ParseIdentifier =>
        From<string>(state =>
        {
            var currentState = state;
            var chars = new List<char>();

            // First character must be a letter
            if (currentState.Position >= currentState.Input.Length) return null;
            var firstChar = currentState.Input[currentState.Position];
            if (!char.IsLetter(firstChar)) return null;

            chars.Add(firstChar);
            currentState = new ParseState(currentState.Input, currentState.Position + 1);

            // Subsequent characters can be letters or digits
            while (currentState.Position < currentState.Input.Length)
            {
                var currentChar = currentState.Input[currentState.Position];
                if (!char.IsLetterOrDigit(currentChar)) break;

                chars.Add(currentChar);
                currentState = new ParseState(currentState.Input, currentState.Position + 1);
            }

            return new ParseResult<string>(new string(chars.ToArray()), currentState);
        });

    public static IStatefulParser<T> Alternative<T>(
        IStatefulParser<T> first,
        IStatefulParser<T> second) =>
        From<T>(state =>
        {
            var firstResult = first.Parse(state);
            return firstResult ?? second.Parse(state);
        });
}

// Fluent parser builder for stateful parsers
public class StatefulParserBuilder<T>
    where T : class
{
    private readonly IStatefulParser<T> _parser;

    public StatefulParserBuilder(IStatefulParser<T> parser)
    {
        _parser = parser;
    }

    public StatefulParserBuilder<TResult> Map<TResult>(Func<T, TResult> mapper)
        where TResult : class =>
        new StatefulParserBuilder<TResult>(
            StatefulParser.From<TResult>(state =>
            {
                var result = _parser.Parse(state);
                return result != null
                    ? new ParseResult<TResult>(mapper(result.Value.Result), result.Value.NewState)
                    : null;
            })
        );

    public StatefulParserBuilder<T> Or(IStatefulParser<T> alternative) =>
        new StatefulParserBuilder<T>(StatefulParser.Alternative(_parser, alternative));

    public T? Parse(string input)
    {
        var result = _parser.Parse(new ParseState(input));
        return result?.Result;
    }
}

// Factory function for fluent interface
public static class StatefulParserBuilder
{
    public static StatefulParserBuilder<T> From<T>(IStatefulParser<T> parser)
        where T : class =>
        new StatefulParserBuilder<T>(parser);
}

// Example usage and tests
public class ParserCombinatorExample
{
    public static void Main()
    {
        TestBasicParsers();
        TestStatefulParsers();
        TestFluentParsers();
    }

    private static void TestBasicParsers()
    {
        Console.WriteLine("Testing basic parsers:");

        var charParserA = BasicParsers.CharParser('a');
        Console.WriteLine($"CharParser('a').Parse(\"abc\"): {charParserA.Parse("abc")}"); // "a"

        var parseFirstDigit = BasicParsers.ParseFirstDigit;
        Console.WriteLine($"ParseFirstDigit.Parse(\"5abc\"): {parseFirstDigit.Parse("5abc")}"); // "5"

        var parseSpecificDigit = BasicParsers.CharParser('7').Then(BasicParsers.DigitParser);
        Console.WriteLine($"ParseSpecificDigit.Parse(\"7\"): {parseSpecificDigit.Parse("7")}"); // "7"
    }

    private static void TestStatefulParsers()
    {
        Console.WriteLine("\nTesting stateful parsers:");

        var testState = new ParseState("123abc");
        var numberResult = StatefulParser.ParseNumber.Parse(testState);
        Console.WriteLine($"ParseNumber(\"123abc\"): Result={numberResult?.Result.Value}, Position={numberResult?.NewState.Position}");

        var idState = new ParseState("hello123");
        var idResult = StatefulParser.ParseIdentifier.Parse(idState);
        Console.WriteLine($"ParseIdentifier(\"hello123\"): Result={idResult?.Result}, Position={idResult?.NewState.Position}");

        // Test alternative parser with same types
        var numberOrIdentifierLength = StatefulParser.Alternative(
            StatefulParser.ParseNumber,
            StatefulParser.From<StatefulParser.IntValue>(state =>
            {
                var idResult = StatefulParser.ParseIdentifier.Parse(state);
                if (idResult == null) return null;

                return new ParseResult<StatefulParser.IntValue>(
                    new StatefulParser.IntValue(idResult.Value.Result.Length),
                    idResult.Value.NewState);
            })
        );

        Console.WriteLine($"NumberOrIdentifierLength(\"123abc\"): {numberOrIdentifierLength.Parse(new ParseState("123abc"))?.Result.Value}"); // 123
        Console.WriteLine($"NumberOrIdentifierLength(\"hello123\"): {numberOrIdentifierLength.Parse(new ParseState("hello123"))?.Result.Value}"); // 8
    }

    private static void TestFluentParsers()
    {
        Console.WriteLine("\nTesting fluent parsers:");

        var numberParser = StatefulParserBuilder.From(StatefulParser.ParseNumber)
            .Map(n => new StatefulParser.IntValue(n.Value * 2));
        Console.WriteLine($"NumberParser(\"42abc\"): {numberParser.Parse("42abc")?.Value}"); // 84

        var stringParser = StatefulParserBuilder.From(StatefulParser.ParseIdentifier)
            .Map(s => s.ToUpper());
        Console.WriteLine($"StringParser(\"hello123\"): {stringParser.Parse("hello123")}"); // HELLO123

        var combinedParser = StatefulParserBuilder.From(StatefulParser.ParseNumber)
            .Map(n => n.Value.ToString())
            .Or(StatefulParser.ParseIdentifier);
        Console.WriteLine($"CombinedParser(\"42abc\"): {combinedParser.Parse("42abc")}"); // "42"
        Console.WriteLine($"CombinedParser(\"hello123\"): {combinedParser.Parse("hello123")}"); // "hello123"
    }
}
