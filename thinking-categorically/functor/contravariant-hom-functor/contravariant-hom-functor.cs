using System;
using System.Collections.Generic;
using System.Linq;

// Contravariant Hom functor implementation
public interface IContravariant<in A>
{
    IContravariant<B> Contramap<B>(Func<B, A> f);
}

// Predicate contravariant functor
public class Predicate<A> : IContravariant<A>
{
    private readonly Func<A, bool> predicate;

    public Predicate(Func<A, bool> predicate)
    {
        this.predicate = predicate;
    }

    public bool Test(A value) => predicate(value);

    public IContravariant<B> Contramap<B>(Func<B, A> f)
    {
        return new Predicate<B>(b => predicate(f(b)));
    }

    // Helper method for easier chaining
    public Predicate<B> ContramapTo<B>(Func<B, A> f)
    {
        return new Predicate<B>(b => predicate(f(b)));
    }
}

public class Person
{
    public int Age { get; set; }
    public string? Name { get; set; }
}


class Program
{
    static void Main()
    {
        // Base predicates (functions targeting bool)
        var isPositive = new Predicate<int>(x => x > 0);
        var isLongString = new Predicate<string>(s => s.Length > 5);

        // Contravariant mapping: int -> bool becomes string -> bool
        var stringIsPositive = isPositive.ContramapTo<string>(s => int.Parse(s));

        Console.WriteLine(isPositive.Test(5));            // True
        Console.WriteLine(isPositive.Test(-3));           // False
        Console.WriteLine(stringIsPositive.Test("10"));   // True
        Console.WriteLine(stringIsPositive.Test("-5"));   // False

        // Person example
        var isAdult = new Predicate<int>(age => age >= 18);
        var personIsAdult = isAdult.ContramapTo<Person>(p => p.Age);

        var alice = new Person { Age = 25, Name = "Alice" };
        var bob = new Person { Age = 16, Name = "Bob" };

        Console.WriteLine($"Alice is adult: {personIsAdult.Test(alice)}"); // True
        Console.WriteLine($"Bob is adult: {personIsAdult.Test(bob)}");     // False
   }
}