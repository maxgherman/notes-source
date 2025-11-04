// In C#, this delegate type represents a hom-set
Func<string, int> // represents Hom(string, int)

// All possible functions from string to int:
Func<string, int> parser = s => int.Parse(s);
Func<string, int> length = s => s.Length;
Func<string, int> hash = s => s.GetHashCode();

// The type Func<string, int> IS the hom-set Hom(string, int)