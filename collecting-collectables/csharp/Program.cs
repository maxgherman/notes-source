public sealed class HArray
{
    public sealed class Key<T>
    {
        internal HArray Owner { get; }
        internal int Index { get; }

        internal Key(HArray owner, int index)
        {
            Owner = owner;
            Index = index;
        }
    }

    private readonly List<object?> values = [];

    public Key<T> Add<T>(T value)
    {
        var key = new Key<T>(this, values.Count);
        values.Add(value);
        return key;
    }

    public T Get<T>(Key<T> key)
    {
        EnsureOwner(key);
        return (T)values[key.Index]!;
    }

    public void Set<T>(Key<T> key, T value)
    {
        EnsureOwner(key);
        values[key.Index] = value;
    }

    private void EnsureOwner<T>(Key<T> key)
    {
        if (!ReferenceEquals(key.Owner, this))
        {
            throw new ArgumentException(
                "The key belongs to another collection.",
                nameof(key));
        }
    }
}

public static class Program
{
    public static void Main()
    {
        var xs = new HArray();

        var name = xs.Add("hello"); // Key<string>
        var age = xs.Add(42);       // Key<int>
        var active = xs.Add(true);  // Key<bool>

        string a = xs.Get(name);
        int b = xs.Get(age);
        bool c = xs.Get(active);

        xs.Set(name, "world");
        xs.Set(age, 100);
        xs.Set(active, false);

        // This does not compile because age is a Key<int>:
        // xs.Set(age, "hello");

        Console.WriteLine($"{a} -> {xs.Get(name)}");
        Console.WriteLine($"{b} -> {xs.Get(age)}");
        Console.WriteLine($"{c} -> {xs.Get(active)}");
    }
}
