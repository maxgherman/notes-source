Func<A, C> Compose<A, B, C>(Func<B, C> g, Func<A, B> f)
    => x => g(f(x));

Func<int, string> f = x => x.ToString();
Func<string, bool> g = s => s.Length > 2;

var h = Compose(g, f); // Func<int, bool>
Console.WriteLine(h(123)); // Output: True
