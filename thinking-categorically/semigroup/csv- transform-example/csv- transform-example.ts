// Semigroup interface
interface Semigroup<T> {
  combine(a: T, b: T): T;
}

// CSV parsing function (as provided)
function readCSV(data: string): Person[] {
  const [header, ...rows] = data.trim().split('\n');
  const keys = header.split(',');
  return rows.map(row => {
    const values = row.split(',');
    const obj = Object.fromEntries(keys.map((k, i) => [k, values[i]])) as any;
    return {
      name: obj.name,
      age: obj.age,
      totalAge: undefined,
      count: undefined
    } as Person;
  });
}

// Person type for type safety
interface Person {
  name: string;
  age: string;
  totalAge?: number;
  count?: number;
}

// Semigroup for combining person records
const PersonSemigroup: Semigroup<Person> = {
  combine: (p1, p2) => ({
    name: p1.name + " & " + p2.name,  // Combine names
    age: Math.max(parseInt(p1.age), parseInt(p2.age)).toString(), // Take max age
    totalAge: (p1.totalAge || parseInt(p1.age)) + (p2.totalAge || parseInt(p2.age)),
    count: (p1.count || 1) + (p2.count || 1)
  })
};

// Semigroup for aggregating ages (sum)
const AgeSumSemigroup: Semigroup<number> = {
  combine: (a, b) => a + b
};

// Semigroup for collecting names
const NameListSemigroup: Semigroup<string[]> = {
  combine: (a, b) => [...a, ...b]
};

// Generic combine function for arrays
function combineAll<T>(semigroup: Semigroup<T>, items: T[]): T {
  if (items.length === 0) {
    throw new Error("Cannot combine empty array");
 }

  return items.reduce(semigroup.combine);
}

// Example usage
const data = readCSV('name,age\nAlice,25\nBob,40\nCharlie,35');

console.log("Original data:", data);

// 1. Combine all persons into one record
const combinedPerson = combineAll(PersonSemigroup, data);
console.log("Combined person:", combinedPerson);

// 2. Sum all ages using semigroup
const ages = data.map(person => parseInt(person.age));
const totalAge = combineAll(AgeSumSemigroup, ages);
console.log("Total age:", totalAge);

// 3. Collect all names using semigroup
const nameArrays = data.map(person => [person.name]);
const allNames = combineAll(NameListSemigroup, nameArrays);
console.log("All names:", allNames);

// 4. Process multiple CSV datasets and combine them
const data2 = readCSV('name,age\nDave,28\nEve,32');
const data3 = readCSV('name,age\nFrank,45\nGrace,29');

// Combine datasets using array concatenation semigroup
const ArraySemigroup = <T>(): Semigroup<T[]> => ({
  combine: (a, b) => [...a, ...b]
});

const allDatasets = combineAll(ArraySemigroup<Person>(), [data, data2, data3]);
console.log("Combined datasets:", allDatasets);

// 5. Create summary statistics using semigroups
interface Stats {
  totalAge: number;
  count: number;
  names: string[];
}

const StatsSemigroup: Semigroup<Stats> = {
  combine: (s1, s2) => ({
    totalAge: s1.totalAge + s2.totalAge,
    count: s1.count + s2.count,
    names: [...s1.names, ...s2.names]
  })
};

const statsFromData = (persons: Person[]): Stats[] =>
  persons.map(p => ({
    totalAge: parseInt(p.age),
    count: 1,
    names: [p.name]
  }));

const overallStats = combineAll(StatsSemigroup, statsFromData(allDatasets));
console.log("Overall statistics:", overallStats);
console.log("Average age:", overallStats.totalAge / overallStats.count);
