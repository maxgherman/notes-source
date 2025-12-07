using System;

// Terminal Object: Unit struct (equivalent to Haskell's ())
public struct Unit
{
    public static readonly Unit Value = new Unit();

    // Every type has exactly one function to Unit
    public static Unit ToTerminal<T>(T value) => Value;
}

// Binary Products: Generic tuple
public struct Product<A, B>
{
    public A First { get; }
    public B Second { get; }

    public Product(A first, B second)
    {
        First = first;
        Second = second;
    }

    // Projections
    public static A Fst(Product<A, B> product) => product.First;
    public static B Snd(Product<A, B> product) => product.Second;
}

// Exponential Objects: Generic delegates
public delegate TResult Exponential<T, TResult>(T input);

public static class CartesianClosedCategory
{
    // Product construction
    public static Product<A, B> MakePair<A, B>(A first, B second) =>
        new Product<A, B>(first, second);

    // Universal property: pairing morphism
    public static Func<X, Product<A, B>> Pairing<X, A, B>(
        Func<X, A> f,
        Func<X, B> g) =>
        x => new Product<A, B>(f(x), g(x));

    // Evaluation morphism
    public static TResult Eval<T, TResult>(
        Exponential<T, TResult> func,
        T argument) =>
        func(argument);

    // Alternative eval that takes a product
    public static TResult EvalProduct<T, TResult>(
        Product<Exponential<T, TResult>, T> pair) =>
        pair.First(pair.Second);

    // Currying: ((A × B) → C) → (A → (B → C))
    public static Func<A, Func<B, C>> Curry<A, B, C>(
        Func<Product<A, B>, C> f) =>
        a => b => f(new Product<A, B>(a, b));

    // Uncurrying: (A → (B → C)) → ((A × B) → C)
    public static Func<Product<A, B>, C> Uncurry<A, B, C>(
        Func<A, Func<B, C>> f) =>
        pair => f(pair.First)(pair.Second);

    // Function composition
    public static Exponential<A, C> Compose<A, B, C>(
        Exponential<B, C> f,
        Exponential<A, B> g) =>
        x => f(g(x));
}

class Program
{
    static void Main()
    {
        // Original function: Product<int, int> -> int
        Func<Product<int, int>, int> multiply =
            pair => pair.First * pair.Second;

        // Curry it: int -> (int -> int)
        var curriedMultiply = CartesianClosedCategory.Curry(multiply);

        // Uncurry it back: Product<int, int> -> int
        var uncurriedMultiply = CartesianClosedCategory.Uncurry(curriedMultiply);

        // Test the isomorphism
        var testPair = new Product<int, int>(6, 7);

        Console.WriteLine($"Original: {multiply(testPair)}");           // 42
        Console.WriteLine($"Curried: {curriedMultiply(6)(7)}");        // 42
        Console.WriteLine($"Uncurried: {uncurriedMultiply(testPair)}"); // 42

        // Partial application demonstrates exponential objects
        var multiplyBy5 = curriedMultiply(5);
        Console.WriteLine($"5 × 8 = {multiplyBy5(8)}"); // 40

        // Higher-order functions
        Exponential<int, int> addOne = x => x + 1;
        Exponential<int, int> double_ = x => x * 2;
        Exponential<int, string> toString = x => x.ToString();

        // Function composition
        var pipeline = CartesianClosedCategory.Compose(
            toString,
            CartesianClosedCategory.Compose(double_, addOne)
        );

        Console.WriteLine($"Pipeline(5) = {pipeline(5)}"); // "12"

        // Evaluation morphism
        Console.WriteLine($"Eval(addOne, 10) = {CartesianClosedCategory.Eval(addOne, 10)}"); // 11

        // Using pairing morphism
        var coordPairing = CartesianClosedCategory.Pairing<int, int, int>(
            x => x + 1,    // f: int -> int
            x => x * 2     // g: int -> int
        );

        var coords = coordPairing(5);
        Console.WriteLine($"Pairing(5) = ({coords.First}, {coords.Second})"); // (6, 10)
    }
}