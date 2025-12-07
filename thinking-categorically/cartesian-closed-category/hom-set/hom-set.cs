using System;
using System.Collections.Generic;
using System.Linq;

// The Hom-set Hom(A, B) corresponds to delegate types
public delegate TResult Hom<T, TResult>(T input);

// In a CCC: Hom(A, B) ≅ B^A (exponential object)
public delegate TResult Exponential<T, TResult>(T input);

// Product type for demonstrating currying
public struct Product<A, B>
{
    public A First { get; }
    public B Second { get; }

    public Product(A first, B second)
    {
        First = first;
        Second = second;
    }
}

public static class HomSetExamples
{
    // The currying isomorphism: Hom(A × B, C) ≅ Hom(A, Hom(B, C))
    public static void CurryingIsomorphism()
    {
        Console.WriteLine("=== Currying Isomorphism ===");

        // Left side: Hom(Product<int, int>, int)
        Hom<Product<int, int>, int> multiply =
            pair => pair.First * pair.Second;

        // Right side: Hom(int, Hom(int, int))
        Hom<int, Hom<int, int>> curryMultiply =
            x => y => x * y;

        // Demonstrate the isomorphism
        var testPair = new Product<int, int>(6, 7);
        Console.WriteLine($"multiply(6,7) = {multiply(testPair)}");           // 42
        Console.WriteLine($"curryMultiply(6)(7) = {curryMultiply(6)(7)}");   // 42

        // curry/uncurry witness the isomorphism
        var curried = Curry(multiply);
        var uncurried = Uncurry(curryMultiply);

        Console.WriteLine($"curried(6)(7) = {curried(6)(7)}");               // 42
        Console.WriteLine($"uncurried(6,7) = {uncurried(testPair)}");        // 42
    }

    // Curry: Hom(A × B, C) -> Hom(A, Hom(B, C))
    public static Hom<A, Hom<B, C>> Curry<A, B, C>(Hom<Product<A, B>, C> f) =>
        a => b => f(new Product<A, B>(a, b));

    // Uncurry: Hom(A, Hom(B, C)) -> Hom(A × B, C)
    public static Hom<Product<A, B>, C> Uncurry<A, B, C>(Hom<A, Hom<B, C>> f) =>
        pair => f(pair.First)(pair.Second);

    // Higher-order functions manipulate Hom-sets
    public static IEnumerable<Hom<A, C>> MapHom<A, B, C>(
        Hom<B, C> f,
        IEnumerable<Hom<A, B>> functions) =>
        functions.Select(g => new Hom<A, C>(x => f(g(x))));

    // Example: Working with collections of functions
    public static void FunctionManipulation()
    {
        Console.WriteLine("\n=== Function Manipulation ===");

        // Collection of functions (elements of Hom(int, int))
        var numberFunctions = new List<Hom<int, int>>
        {
            x => x + 1,
            x => x * 2,
            x => x * x
        };

        // Transform with another function (Hom(int, string))
        Hom<int, string> toString = x => $"Result: {x}";

        // Get new functions (elements of Hom(int, string))
        var stringFunctions = MapHom(toString, numberFunctions).ToList();

        // Test the transformed functions
        for (int i = 0; i < stringFunctions.Count; i++)
        {
            Console.WriteLine($"Function {i}(5) = {stringFunctions[i](5)}");
        }
    }

    // Exponential objects as function spaces
    public static void ExponentialObjects()
    {
        Console.WriteLine("\n=== Exponential Objects ===");

        // string^int represents all functions from int to string
        var stringFromInt = new List<Exponential<int, string>>
        {
            x => x.ToString(),
            x => $"The number {x}",
            x => new string('*', Math.Max(0, x)),
            x => x < 0 ? "negative" : x > 0 ? "positive" : "zero"
        };

        // Functions are first-class - can be stored, passed, returned
        Hom<int, Exponential<int, string>> selectFunction =
            index => index < stringFromInt.Count ? stringFromInt[index] :
                     (x => $"Unknown: {x}");

        var selectedFunc = selectFunction(2);
        Console.WriteLine($"Selected function(5) = {selectedFunc(5)}"); // *****

        // Function composition creates new exponential objects
        Hom<string, string> addPrefix = s => $"Output: {s}";
        var composedFunctions = stringFromInt
            .Select(f => new Exponential<int, string>(x => addPrefix(f(x))))
            .ToList();

        Console.WriteLine($"Composed function(3) = {composedFunctions[1](3)}");
        // "Output: The number 3"
    }

    // Natural transformations between Hom-sets
    // eta: ∀A,B. A -> Hom(B, A)  (constant function)
    public static Hom<B, A> Eta<A, B>(A value) => _ => value;

    // Demonstrating the category structure of Hom-sets
    public static void HomCategory()
    {
        Console.WriteLine("\n=== Hom-sets Category ===");

        // Identity natural transformation: Hom(A,B) -> Hom(A,B)
        Hom<Hom<int, string>, Hom<int, string>> idHom = f => f;

        // Example of natural transformation: pre-composition
        // Given g: A -> B, get (g*): Hom(B,C) -> Hom(A,C)
        // where (g*)(f) = f ∘ g
        Hom<int, int> doubleIt = x => x * 2;
        Hom<Hom<int, string>, Hom<int, string>> preCompose =
            f => x => f(doubleIt(x));

        Hom<int, string> originalFunction = x => $"Value: {x}";
        var transformedFunction = preCompose(originalFunction);

        Console.WriteLine($"Original(5) = {originalFunction(5)}");      // "Value: 5"
        Console.WriteLine($"Transformed(5) = {transformedFunction(5)}"); // "Value: 10"

        Console.WriteLine("Hom-sets form a category with natural transformations as morphisms");
    }

    // The fundamental insight: Hom(A,B) ≅ B^A
    public static void FundamentalTheorem()
    {
        // Demonstrate that function types are both morphisms AND objects
        Hom<int, string> functionAsMorphism = x => x.ToString();  // Morphism in Hom(int, string)

        // The same function type as an object that can be manipulated
        var functionsAsObjects = new List<Hom<int, string>>
        {
            functionAsMorphism,  // Our function is now data!
            x => $"Number: {x}",
            x => new string('X', x)
        };

        // Higher-order function that takes functions as arguments (objects)
        // and returns a function (object) - this is only possible because
        // Hom(A,B) ≅ B^A makes functions first-class
        Hom<Hom<int, string>, string> testFunction = f => f(42);

        foreach (var func in functionsAsObjects)
        {
            Console.WriteLine($"Testing function: {testFunction(func)}");
        }
    }
}

class Program
{
    static void Main()
    {
        Console.WriteLine("Hom-sets Connection to Cartesian Closed Categories\n");

        HomSetExamples.CurryingIsomorphism();
        HomSetExamples.FunctionManipulation();
        HomSetExamples.ExponentialObjects();
        HomSetExamples.HomCategory();
        HomSetExamples.FundamentalTheorem();
    }
}