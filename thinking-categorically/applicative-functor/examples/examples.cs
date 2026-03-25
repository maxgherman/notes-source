using System;

public class F<A>
{
    public A Value { get; }

    public F(A value)
    {
        Value = value;
    }

    // Functor map: (A → B) → F<A> → F<B>
    public F<B> Map<B>(Func<A, B> f)
    {
        return new F<B>(f(Value));
    }

    // Applicative ap: F<Func<A, B>> → F<A> → F<B>
    public F<B> Ap<B>(F<Func<A, B>> fab)
    {
        return new F<B>(fab.Value(Value));
    }

    public override string ToString()
    {
        return $"F {{ Value = {Value} }}";
    }
}

class Program
{
    static void Main()
    {
        var result = new F<int>(1);

        var mapped = result
            .Map(x => x + 1)
            .Map(x => $"{x} + 1");

        Console.WriteLine(mapped); // F { Value = 2 + 1 }

        var applied = result.Ap(new F<Func<int, int>>(x => x + 1));
        Console.WriteLine(applied); // F { Value = 2 }
    }
}
