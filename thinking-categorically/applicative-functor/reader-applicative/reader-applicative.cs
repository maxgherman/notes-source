using System;

public class Reader<R, A>
{
    public Func<R, A> run { get; }

    public Reader(Func<R, A> run)
    {
        this.run = run;
    }

    // Run the computation with a given environment
    public A Run(R env) => run(env);

    // Functor: map :: (A -> B) -> Reader<R, A> -> Reader<R, B>
    public Reader<R, B> Map<B>(Func<A, B> f)
    {
        return new Reader<R, B>(r => f(run(r)));
    }

    // Applicative: ap :: Reader<R, (A -> B)> -> Reader<R, A> -> Reader<R, B>
    public Reader<R, B> Ap<B>(Reader<R, Func<A, B>> rf)
    {
        return new Reader<R, B>(r =>
        {
            var func = rf.run(r);  // Func<A, B>
            var val = run(r);      // A
            return func(val);      // B
        });
    }

    // Applicative: pure :: A -> Reader<R, A>
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
        var readerF = new Reader<Env, Func<int, int>>(env => x => x * env.Multiplier);

        var resultReader = readerA.Ap(readerF);

        var result = resultReader.Run(new Env { Multiplier = 3 });  // (5 + 3) * 3 = 24
        Console.WriteLine(result); // Output: 24
    }
}
