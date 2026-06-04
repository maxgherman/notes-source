var computation =
    Maybe.Pure(20)
        .SelectMany(x => SafeDivide(x, 4))
        .SelectMany(y => SafeDivide(y, 2));

Console.WriteLine($"Bind result: {computation}");

var linqResult =
    from x in Maybe.Pure(20)
    from y in SafeDivide(x, 4)
    from z in SafeDivide(y, 2)
    select z;

Console.WriteLine($"LINQ result: {linqResult}");

static Maybe<int> SafeDivide(int x, int y)
{
    return y == 0 ? Maybe<int>.None : Maybe.Pure(x / y);
}

public sealed class Maybe<T>
{
    private Maybe(T? value, bool hasValue)
    {
        Value = value;
        HasValue = hasValue;
    }

    public T? Value { get; }

    public bool HasValue { get; }

    public static Maybe<T> None { get; } = new(default, false);

    public static Maybe<T> Pure(T value)
    {
        return new Maybe<T>(value, true);
    }

    public override string ToString()
    {
        return HasValue ? $"Some({Value})" : "None";
    }
}

public static class Maybe
{
    public static Maybe<T> Pure<T>(T value)
    {
        return Maybe<T>.Pure(value);
    }
}

public static class MaybeExtensions
{
    public static Maybe<TResult> Bind<T, TResult>(
        this Maybe<T> maybe,
        Func<T, Maybe<TResult>> selector)
    {
        return maybe.HasValue ? selector(maybe.Value!) : Maybe<TResult>.None;
    }

    public static Maybe<TResult> Select<T, TResult>(
        this Maybe<T> maybe,
        Func<T, TResult> selector)
    {
        return maybe.Bind(value => Maybe.Pure(selector(value)));
    }

    public static Maybe<TResult> SelectMany<T, TResult>(
        this Maybe<T> maybe,
        Func<T, Maybe<TResult>> selector)
    {
        return maybe.Bind(selector);
    }

    public static Maybe<TResult> SelectMany<T, TIntermediate, TResult>(
        this Maybe<T> maybe,
        Func<T, Maybe<TIntermediate>> selector,
        Func<T, TIntermediate, TResult> projector)
    {
        return maybe.Bind(value =>
            selector(value).Bind(intermediate => Maybe.Pure(projector(value, intermediate))));
    }
}
