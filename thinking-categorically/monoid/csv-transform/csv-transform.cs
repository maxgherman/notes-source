using System;
using System.Collections.Generic;
using System.Linq;

// Enhanced monoid interface with identity element
public interface IMonoid<T>
{
    T Empty { get; }
    T Combine(T a, T b);
}

// Person type with safe defaults
public class Person
{
    public string Name { get; set; } = "";
    public string Age { get; set; } = "0";
    public int? TotalAge { get; set; }
    public int? Count { get; set; }
}

// Person monoid with safe empty person
public class PersonMonoid : IMonoid<Person>
{
    public Person Empty => new Person { Name = "", Age = "0", TotalAge = 0, Count = 0 };

    public Person Combine(Person p1, Person p2)
    {
        if (p1.Name == "" && p1.Age == "0") return p2;
        if (p2.Name == "" && p2.Age == "0") return p1;

        return new Person
        {
            Name = $"{p1.Name} & {p2.Name}",
            Age = Math.Max(int.Parse(p1.Age), int.Parse(p2.Age)).ToString(),
            TotalAge = (p1.TotalAge ?? int.Parse(p1.Age)) + (p2.TotalAge ?? int.Parse(p2.Age)),
            Count = (p1.Count ?? 1) + (p2.Count ?? 1)
        };
    }
}

// Age sum monoid (safe for empty collections)
public class AgeSumMonoid : IMonoid<int>
{
    public int Empty => 0;
    public int Combine(int a, int b) => a + b;
}

// Name collection monoid (safe concatenation)
public class NameListMonoid : IMonoid<List<string>>
{
    public List<string> Empty => new List<string>();
    public List<string> Combine(List<string> a, List<string> b) => a.Concat(b).ToList();
}

// Array concatenation monoid (generic)
public class ArrayMonoid<T> : IMonoid<List<T>>
{
    public List<T> Empty => new List<T>();
    public List<T> Combine(List<T> a, List<T> b) => a.Concat(b).ToList();
}

// Statistics monoid for comprehensive data analysis
public class Stats
{
    public int TotalAge { get; set; }
    public int Count { get; set; }
    public List<string> Names { get; set; } = new List<string>();
    public double AverageAge { get; set; }
}

public class StatsMonoid : IMonoid<Stats>
{
    public Stats Empty => new Stats { TotalAge = 0, Count = 0, Names = new List<string>(), AverageAge = 0 };

    public Stats Combine(Stats s1, Stats s2)
    {
        var totalAge = s1.TotalAge + s2.TotalAge;
        var count = s1.Count + s2.Count;
        return new Stats
        {
            TotalAge = totalAge,
            Count = count,
            Names = s1.Names.Concat(s2.Names).ToList(),
            AverageAge = count > 0 ? (double)totalAge / count : 0
        };
    }
}

public class Program
{
    // CSV parsing function (enhanced for safety)
    public static List<Person> ReadCSV(string data)
    {
      if (string.IsNullOrWhiteSpace(data)) return new List<Person>();

      var lines = data.Trim().Split('\n');
      if (lines.Length < 2) return new List<Person>();

      var header = lines[0].Split(',');
      var rows = lines.Skip(1);

      var result = new List<Person>();
      foreach (var row in rows)
      {
          var values = row.Split(',');
          var dict = new Dictionary<string, string>();
          for (int i = 0; i < header.Length && i < values.Length; i++)
              dict[header[i]] = values[i];

          result.Add(new Person
          {
              Name = dict.ContainsKey("name") ? dict["name"] : "",
              Age = dict.ContainsKey("age") ? dict["age"] : "0",
              TotalAge = null,
              Count = null
          });
      }
      return result;
    }

    // Safe fold function using monoids
    public static T Fold<T>(IMonoid<T> monoid, IEnumerable<T> items)
    {
        return items.Aggregate(monoid.Empty, monoid.Combine);
    }

    // Safe map-fold operation
    public static B FoldMap<A, B>(IMonoid<B> monoid, Func<A, B> mapper, IEnumerable<A> items)
    {
        return Fold(monoid, items.Select(mapper));
    }

    public static void Main()
    {
        var data1 = ReadCSV("name,age\nAlice,25\nBob,40\nCharlie,35");
        var data2 = ReadCSV("name,age\nDave,28\nEve,32");
        var data3 = ReadCSV(""); // Empty CSV - safe with monoids!
        var data4 = ReadCSV("name,age\nFrank,45\nGrace,29");

        Console.WriteLine("Dataset 1: " + string.Join("; ", data1.Select(p => $"{p.Name}:{p.Age}")));
        Console.WriteLine("Dataset 2: " + string.Join("; ", data2.Select(p => $"{p.Name}:{p.Age}")));
        Console.WriteLine("Dataset 3 (empty): " + string.Join("; ", data3.Select(p => $"{p.Name}:{p.Age}")));

        // 1. Safe combination of all persons (empty datasets don't break anything)
        var personMonoid = new PersonMonoid();
        var combinedPerson = Fold(personMonoid, new[]
        {
            Fold(personMonoid, data1),
            Fold(personMonoid, data2),
            Fold(personMonoid, data3),
            Fold(personMonoid, data4)
        });
        Console.WriteLine("Combined person: " + $"{combinedPerson.Name}, Age: {combinedPerson.Age}, TotalAge: {combinedPerson.TotalAge}, Count: {combinedPerson.Count}");

        // 2. Safe age summation (works even with empty datasets)
        var allAges = data1.Concat(data2).Concat(data3).Concat(data4).Select(p => int.Parse(p.Age)).ToList();
        var ageSumMonoid = new AgeSumMonoid();
        var totalAge = Fold(ageSumMonoid, allAges);
        Console.WriteLine("Total age: " + totalAge);

        // 3. Safe name collection
        var nameListMonoid = new NameListMonoid();
        var allNames = FoldMap(nameListMonoid, (Person p) => new List<string> { p.Name }, data1.Concat(data2).Concat(data3).Concat(data4));
        Console.WriteLine("All names: " + string.Join(", ", allNames));

        // 4. Safe dataset merging with automatic empty handling
        var arrayMonoid = new ArrayMonoid<Person>();
        var allDatasets = Fold(arrayMonoid, new List<List<Person>> { data1, data2, data3, data4 });
        Console.WriteLine("Combined datasets: " + string.Join("; ", allDatasets.Select(p => $"{p.Name}:{p.Age}")));
        Console.WriteLine("Total records: " + allDatasets.Count);

        // 5. Comprehensive statistics with safe empty handling
        Stats StatsFromPerson(Person p) => new Stats
        {
            TotalAge = int.Parse(p.Age),
            Count = 1,
            Names = new List<string> { p.Name },
            AverageAge = int.Parse(p.Age)
        };

        var statsMonoid = new StatsMonoid();
        var overallStats = FoldMap(statsMonoid, StatsFromPerson, allDatasets);
        Console.WriteLine($"Overall statistics: TotalAge={overallStats.TotalAge}, Count={overallStats.Count}, AverageAge={overallStats.AverageAge}, Names=[{string.Join(", ", overallStats.Names)}]");

        // 6. Safe pipeline operations
        Stats ProcessDatasetSafely(string csvData)
        {
            var persons = ReadCSV(csvData);
            return FoldMap(statsMonoid, StatsFromPerson, persons);
        }

        var results = new[]
        {
            ProcessDatasetSafely("name,age\nAlice,25"),
            ProcessDatasetSafely(""),
            ProcessDatasetSafely("name,age"),
            ProcessDatasetSafely("name,age\nBob,30\nCharlie,35")
        }.Select(result => Fold(statsMonoid, new[] { result })).ToList();

        var finalResult = Fold(statsMonoid, results);
        Console.WriteLine($"Pipeline result: TotalAge={finalResult.TotalAge}, Count={finalResult.Count}, AverageAge={finalResult.AverageAge}, Names=[{string.Join(", ", finalResult.Names)}]");

        // 7. Error recovery with identity elements
        Stats ProcessWithRetry(IEnumerable<string> csvInputs)
        {
            var validResults = new List<Stats>();
            foreach (var csv in csvInputs)
            {
                try
                {
                    var result = ProcessDatasetSafely(csv);
                    validResults.Add(result);
                }
                catch
                {
                    validResults.Add(statsMonoid.Empty);
                    Console.WriteLine("Processing failed, using identity element");
                }
            }
            return Fold(statsMonoid, validResults);
        }

        var robustResult = ProcessWithRetry(new[]
        {
            "name,age\nAlice,25",
            "",
            "invalid,csv,format",
            "name,age\nBob,30"
        });

        Console.WriteLine($"Robust processing result: TotalAge={robustResult.TotalAge}, Count={robustResult.Count}, AverageAge={robustResult.AverageAge}, Names=[{string.Join(", ", robustResult.Names)}]");
    }
}
