using System;

public class Reader<R, A>
{
    public Func<R, A> run { get; }

    public Reader(Func<R, A> run)
    {
        this.run = run;
    }

    // Run the computation
    public A Run(R env) => run(env);

    // Functor: Select (map)
    public Reader<R, B> Select<B>(Func<A, B> f)
    {
        return new Reader<R, B>(r => f(run(r)));
    }

    // Monad: SelectMany (bind / flatMap)
    public Reader<R, B> SelectMany<B>(Func<A, Reader<R, B>> f)
    {
        return new Reader<R, B>(r =>
        {
            var a = run(r);
            return f(a).run(r);
        });
    }

    // For LINQ query with projection after SelectMany
    public Reader<R, C> SelectMany<B, C>(Func<A, Reader<R, B>> f, Func<A, B, C> projector)
    {
        return new Reader<R, C>(r =>
        {
            var a = run(r);
            var b = f(a).run(r);
            return projector(a, b);
        });
    }

    // Applicative pure
    public static Reader<R, A> Pure(A value)
    {
        return new Reader<R, A>(_ => value);
    }
}

class Env
{
    public int Multiplier { get; set; }
}

class Program
{
    static void Main()
    {
        var readerA = new Reader<Env, int>(env => 5 + env.Multiplier);
        var readerB = new Reader<Env, int>(env => 10 * env.Multiplier);

        // Using LINQ query syntax
        var combined = from a in readerA
                       from b in readerB
                       select a + b;

        var result = combined.Run(new Env { Multiplier = 3 }); // (5+3) + (10*3) = 8 + 30 = 38
        Console.WriteLine(result);  // Output: 38
    }
}
