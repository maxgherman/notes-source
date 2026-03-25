using System;

public class FArrow<R, A>
{
    private readonly Func<R, A> run;

    public FArrow(Func<R, A> run)
    {
        this.run = run;
    }

    public A Run(R r) => this.run(r);

    // Equivalent to FArrow.of(a)
    public static FArrow<R, A> Of(A value)
    {
        return new FArrow<R, A>(_ => value);
    }

    // Functor map
    public FArrow<R, B> Map<B>(Func<A, B> f)
    {
        return new FArrow<R, B>(r => f(this.run(r)));
    }

    // Applicative ap
    public FArrow<R, B> Ap<B>(FArrow<R, Func<A, B>> fab)
    {
        return new FArrow<R, B>(r =>
        {
            var func = fab.Run(r); // Func<A, B>
            var val = this.run(r);     // A
            return func(val);      // B
        });
    }
}

class Program
{
    static void Main()
    {
        var fa = new FArrow<int, int>(env => env + 1);
        var ff = new FArrow<int, Func<int, int>>(env => x => x * env);

        var result = fa.Ap(ff);
        Console.WriteLine(result.Run(3)); // (3 + 1) * 3 = 12
    }
}
