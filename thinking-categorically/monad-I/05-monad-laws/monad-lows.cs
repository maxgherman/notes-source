
// Left Identity

// Pure(a).Bind(f) == f(a)
static bool LeftIdentityExample<A, B>(A a, Func<A, Maybe<B>> f)
{
    var left = Maybe<A>.Pure(a).Bind(f);
    var right = f(a);
    return left.Equals(right);
}

// Example usage:
Func<int, Maybe<int>> addTen = x => Maybe<int>.Pure(x + 10);
Console.WriteLine(LeftIdentityExample(5, addTen)); // True


// Right identity

// ma.Bind(Pure) == ma
static bool RightIdentityExample<A>(Maybe<A> ma)
{
    var left = ma.Bind(Maybe<A>.Pure);
    var right = ma;
    return left.Equals(right);
}

// Example usage:
Console.WriteLine(RightIdentityExample(Maybe<int>.Pure(42))); // True
Console.WriteLine(RightIdentityExample(new Maybe<int>(null))); // True

// Associativity

// ma.Bind(f).Bind(g) == ma.Bind(x => f(x).Bind(g))
static bool AssociativityExample<A, B, C>(
    Maybe<A> ma,
    Func<A, Maybe<B>> f,
    Func<B, Maybe<C>> g)
{
    var left = ma.Bind(f).Bind(g);
    var right = ma.Bind(x => f(x).Bind(g));
    return left.Equals(right);
}

// Example usage:
Func<int, Maybe<int>> double = x => Maybe<int>.Pure(x * 2);
Func<int, Maybe<string>> toString = x => Maybe<string>.Pure(x.ToString());
Console.WriteLine(AssociativityExample(Maybe<int>.Pure(5), double, toString)); // True

