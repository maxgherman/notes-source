using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;

public class Reader<R, A>
{
    public Func<R, A> run { get; }

    public Reader(Func<R, A> run)
    {
        this.run = run;
    }

    public A Run(R env) => run(env);

    // Functor: map
    public Reader<R, B> Select<B>(Func<A, B> f)
    {
        return new Reader<R, B>(r => f(run(r)));
    }

    // Monad: bind
    public Reader<R, B> SelectMany<B>(Func<A, Reader<R, B>> f)
    {
        return new Reader<R, B>(r =>
        {
            var a = run(r);
            return f(a).run(r);
        });
    }

    // Applicative: ap
    public Reader<R, B> Ap<B>(Reader<R, Func<A, B>> rf)
    {
        return new Reader<R, B>(r =>
        {
            var f = rf.run(r);
            var a = run(r);
            return f(a);
        });
    }

    // Static pure
    public static Reader<R, A> Pure(A value) => new Reader<R, A>(_ => value);
}

class Program
{
    public class Config
    {
        public int AgeThreshold { get; set; }
        public string Locale { get; set; } = "en";
    }

    public class Person
    {
        public string Name { get; set; } = "";
        public int Age { get; set; }
        public string AgeGroup { get; set; } = "";

        public override string ToString() =>
            $"Person {{ Name = \"{Name}\", Age = {Age}, AgeGroup = \"{AgeGroup}\" }}";
    }

    // Reader class from above...

    // Transformations as Readers returning Func<Person->Person>

    static Reader<Config, Func<Person, Person>> ToNumber(string rawAge)
    {
        return Reader<Config, Func<Person, Person>>.Pure(p =>
        {
            p.Age = int.Parse(rawAge, CultureInfo.InvariantCulture);
            return p;
        });
    }

    static Reader<Config, Func<Person, Person>> AddAgeGroup()
    {
        return new Reader<Config, Func<Person, Person>>(config => p =>
        {
            p.AgeGroup = p.Age > config.AgeThreshold ? "old" : "young";
            return p;
        });
    }

    static Reader<Config, Func<Person, Person>> LocalizeName(string rawName)
    {
        return new Reader<Config, Func<Person, Person>>(config => p =>
        {
            p.Name = config.Locale == "de"
                ? rawName.ToUpperInvariant()
                : rawName.ToLowerInvariant();
            return p;
        });
    }

    // Compose pipeline with Ap

    static Reader<Config, Person> TransformPerson(string rawName, string rawAge)
    {
        var basePerson = Reader<Config, Person>.Pure(new Person());

        var withName = basePerson.Ap(LocalizeName(rawName));
        var withAge = withName.Ap(ToNumber(rawAge));
        var withAgeGroup = withAge.Ap(AddAgeGroup());

        return withAgeGroup;
    }

    // CSV parser

    static List<Dictionary<string, string>> ReadCSV(string csv)
    {
        var lines = csv.Trim().Split(new[] { '\n', '\r' }, StringSplitOptions.RemoveEmptyEntries);
        if (lines.Length < 2) return new List<Dictionary<string, string>>();

        var headers = lines[0].Split(',').Select(h => h.Trim()).ToArray();
        var rows = lines.Skip(1).Select(line =>
        {
            var values = line.Split(',').Select(v => v.Trim()).ToArray();
            if (values.Length != headers.Length)
                throw new Exception($"Malformed row: {line}");

            return headers.Zip(values, (k, v) => new { k, v })
                          .ToDictionary(x => x.k, x => x.v);
        }).ToList();

        return rows;
    }

    static void Main()
    {
        var csv = "name,age\nAlice,25\nBob,40";
        var data = ReadCSV(csv);

        var config = new Config { AgeThreshold = 30, Locale = "de" };

        var people = data.Select(row =>
        {
            var name = row["name"];
            var age = row["age"];
            return TransformPerson(name, age).Run(config);
        }).ToList();

        foreach (var person in people)
            Console.WriteLine(person);
    }
}
