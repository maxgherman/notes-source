using System;
using System.Collections.Generic;
using System.Linq;

class Person
{
    public string? Name { get; set; }
    public int Age { get; set; }
    public string? AgeGroup { get; set; }

    public override string ToString() =>
        $"{{ name: '{Name}', age: {Age}, ageGroup: '{AgeGroup}' }}";
}

class Program
{
    static void Main()
    {
        // Parse CSV file into a dictionary: column name -> list of values
        Dictionary<string, List<string>> ReadCSV(string data)
        {
            var lines = data.Split('\n', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
            var keys = lines[0].Split(',');
            var columns = keys.ToDictionary(k => k, k => new List<string>());

            foreach (var line in lines.Skip(1))
            {
                var values = line.Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
                for (int i = 0; i < keys.Length; i++)
                {
                    columns[keys[i]].Add(values[i]);
                }
            }
            return columns;
        }

        // Transform the dictionary into a list of Person objects
        IEnumerable<Person> TransformData(Dictionary<string, List<string>> data)
        {
            var names = data["name"];
            var ages = data["age"].Select(int.Parse).ToList();

            var people = names.Zip(ages, (name, age) => new Person
            {
                Name = name.ToUpper(),
                Age = age,
                AgeGroup = age > 30 ? "old" : "young"
            }).ToList();

            return people;
        }

        var data = ReadCSV("name,age\nAlice,25\nBob,40");
        var people = TransformData(data);

        foreach (var person in people)
        {
            Console.WriteLine(person);
        }
    }
}