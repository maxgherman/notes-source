using System;
using System.Collections.Generic;
using System.Linq;

// Semigroup interface
public interface ISemigroup<T>
{
    T Combine(T a, T b);
}

// Person class for type safety
public class Person
{
    public string Name { get; set; }
    public string Age { get; set; }
    public int? TotalAge { get; set; }
    public int? Count { get; set; }

    public Person(string name, string age, int? totalAge = null, int? count = null)
    {
        Name = name;
        Age = age;
        TotalAge = totalAge;
        Count = count;
    }
}

// Semigroup for combining person records
public class PersonSemigroup : ISemigroup<Person>
{
    public Person Combine(Person p1, Person p2)
    {
        return new Person(
            name: $"{p1.Name} & {p2.Name}",  // Combine names
            age: Math.Max(int.Parse(p1.Age), int.Parse(p2.Age)).ToString(), // Take max age
            totalAge: (p1.TotalAge ?? int.Parse(p1.Age)) + (p2.TotalAge ?? int.Parse(p2.Age)),
            count: (p1.Count ?? 1) + (p2.Count ?? 1)
        );
    }
}

// Semigroup for aggregating ages (sum)
public class AgeSumSemigroup : ISemigroup<int>
{
    public int Combine(int a, int b) => a + b;
}

// Semigroup for collecting names
public class NameListSemigroup : ISemigroup<List<string>>
{
    public List<string> Combine(List<string> a, List<string> b)
    {
        var result = new List<string>(a);
        result.AddRange(b);
        return result;
    }
}

// Generic array semigroup
public class ArraySemigroup<T> : ISemigroup<List<T>>
{
    public List<T> Combine(List<T> a, List<T> b)
    {
        var result = new List<T>(a);
        result.AddRange(b);
        return result;
    }
}

// Statistics class for aggregating data
public class Stats
{
    public int TotalAge { get; set; }
    public int Count { get; set; }
    public List<string> Names { get; set; }

    public Stats(int totalAge, int count, List<string> names)
    {
        TotalAge = totalAge;
        Count = count;
        Names = names ?? new List<string>();
    }
}

// Semigroup for combining statistics
public class StatsSemigroup : ISemigroup<Stats>
{
    public Stats Combine(Stats s1, Stats s2)
    {
        var combinedNames = new List<string>(s1.Names);
        combinedNames.AddRange(s2.Names);

        return new Stats(
            totalAge: s1.TotalAge + s2.TotalAge,
            count: s1.Count + s2.Count,
            names: combinedNames
        );
    }
}

// Extension methods for semigroup operations
public static class SemigroupExtensions
{
    // Generic combine function for collections
    public static T CombineAll<T>(this ISemigroup<T> semigroup, IEnumerable<T> items)
    {
        var list = items.ToList();
        if (!list.Any())
        {
            throw new ArgumentException("Cannot combine empty collection");
        }

        return list.Aggregate(semigroup.Combine);
    }
}

// CSV parsing utility
public static class CsvParser
{
    public static List<Person> ReadCSV(string data)
    {
        var lines = data.Trim().Split('\n');
        if (lines.Length == 0) return new List<Person>();

        var header = lines[0];
        var rows = lines.Skip(1);
        var keys = header.Split(',');

        return rows.Select(row =>
        {
            var values = row.Split(',');
            var obj = keys.Zip(values, (k, v) => new { Key = k, Value = v })
                         .ToDictionary(x => x.Key, x => x.Value);

            return new Person(
                name: obj["name"],
                age: obj["age"]
            );
        }).ToList();
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Initialize semigroups
        var personSemigroup = new PersonSemigroup();
        var ageSumSemigroup = new AgeSumSemigroup();
        var nameListSemigroup = new NameListSemigroup();
        var arraySemigroup = new ArraySemigroup<Person>();
        var statsSemigroup = new StatsSemigroup();

        // Parse CSV data
        var data = CsvParser.ReadCSV("name,age\nAlice,25\nBob,40\nCharlie,35");

        Console.WriteLine("Original data:");
        foreach (var person in data)
        {
            Console.WriteLine($"  {person.Name}, {person.Age}");
        }

        // 1. Combine all persons into one record
        var combinedPerson = personSemigroup.CombineAll(data);
        Console.WriteLine($"\nCombined person: {combinedPerson.Name}, " +
                         $"age: {combinedPerson.Age}, total age: {combinedPerson.TotalAge}, " +
                         $"count: {combinedPerson.Count}");

        // 2. Sum all ages using semigroup
        var ages = data.Select(person => int.Parse(person.Age));
        var totalAge = ageSumSemigroup.CombineAll(ages);
        Console.WriteLine($"Total age: {totalAge}");

        // 3. Collect all names using semigroup
        var nameArrays = data.Select(person => new List<string> { person.Name });
        var allNames = nameListSemigroup.CombineAll(nameArrays);
        Console.WriteLine($"All names: [{string.Join(", ", allNames)}]");

        // 4. Process multiple CSV datasets and combine them
        var data2 = CsvParser.ReadCSV("name,age\nDave,28\nEve,32");
        var data3 = CsvParser.ReadCSV("name,age\nFrank,45\nGrace,29");

        var allDatasets = arraySemigroup.CombineAll(new[] { data, data2, data3 });
        Console.WriteLine($"\nCombined datasets ({allDatasets.Count} people):");
        foreach (var person in allDatasets)
        {
            Console.WriteLine($"  {person.Name}, {person.Age}");
        }

        // 5. Create summary statistics using semigroups
        var statsFromData = allDatasets.Select(p => new Stats(
            totalAge: int.Parse(p.Age),
            count: 1,
            names: new List<string> { p.Name }
        ));

        var overallStats = statsSemigroup.CombineAll(statsFromData);
        Console.WriteLine($"\nOverall statistics:");
        Console.WriteLine($"  Total age: {overallStats.TotalAge}");
        Console.WriteLine($"  Count: {overallStats.Count}");
        Console.WriteLine($"  Names: [{string.Join(", ", overallStats.Names)}]");
        Console.WriteLine($"  Average age: {(double)overallStats.TotalAge / overallStats.Count:F2}");
    }
}
