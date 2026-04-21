// Enhanced monoid interface with identity element
interface Monoid<T> {
  empty: T;
  combine: (a: T, b: T) => T;
}

// CSV parsing function (enhanced for safety)
function readCSV(data: string): Person[] {
  if (!data.trim()) return []; // Safe handling of empty input

  const [header, ...rows] = data.trim().split('\n');
  if (!header || rows.length === 0) return []; // Safe handling of malformed CSV

  const keys = header.split(',');
  return rows.map(row => {
    const values = row.split(',');
    const obj = Object.fromEntries(keys.map((k, i) => [k, values[i]])) as any;
    return {
      name: obj.name || '',
      age: obj.age || '0',
      totalAge: undefined,
      count: undefined
    } as Person;
  });
}

// Person type with safe defaults
interface Person {
  name: string;
  age: string;
  totalAge?: number;
  count?: number;
}

// Person monoid with safe empty person
const PersonMonoid: Monoid<Person> = {
  empty: {
    name: '',
    age: '0',
    totalAge: 0,
    count: 0
  },
  combine: (p1, p2) => {
    // Handle empty persons gracefully
    if (p1.name === '' && p1.age === '0') return p2;
    if (p2.name === '' && p2.age === '0') return p1;

    return {
      name: p1.name + " & " + p2.name,
      age: Math.max(parseInt(p1.age), parseInt(p2.age)).toString(),
      totalAge: (p1.totalAge || parseInt(p1.age)) + (p2.totalAge || parseInt(p2.age)),
      count: (p1.count || 1) + (p2.count || 1)
    };
  }
};

// Age sum monoid (safe for empty collections)
const AgeSumMonoid: Monoid<number> = {
  empty: 0,
  combine: (a, b) => a + b
};

// Name collection monoid (safe concatenation)
const NameListMonoid: Monoid<string[]> = {
  empty: [],
  combine: (a, b) => [...a, ...b]
};

// Array concatenation monoid (generic)
const ArrayMonoid = <T>(): Monoid<T[]> => ({
  empty: [],
  combine: (a, b) => [...a, ...b]
});

// Statistics monoid for comprehensive data analysis
interface Stats {
  totalAge: number;
  count: number;
  names: string[];
  averageAge: number;
}

const StatsMonoid: Monoid<Stats> = {
  empty: {
    totalAge: 0,
    count: 0,
    names: [],
    averageAge: 0
  },
  combine: (s1, s2) => {
    const totalAge = s1.totalAge + s2.totalAge;
    const count = s1.count + s2.count;
    return {
      totalAge,
      count,
      names: [...s1.names, ...s2.names],
      averageAge: count > 0 ? totalAge / count : 0
    };
  }
};

// Safe fold function using monoids
function fold<T>(monoid: Monoid<T>, items: T[]): T {
  return items.reduce(monoid.combine, monoid.empty);
}

// Safe map-fold operation
function foldMap<A, B>(monoid: Monoid<B>, mapper: (a: A) => B, items: A[]): B {
  return fold(monoid, items.map(mapper));
}

// Usage examples showcasing monoid benefits
const data1 = readCSV('name,age\nAlice,25\nBob,40\nCharlie,35');
const data2 = readCSV('name,age\nDave,28\nEve,32');
const data3 = readCSV(''); // Empty CSV - safe with monoids!
const data4 = readCSV('name,age\nFrank,45\nGrace,29');

console.log("Dataset 1:", data1);
console.log("Dataset 2:", data2);
console.log("Dataset 3 (empty):", data3); // Empty array, handled safely

// 1. Safe combination of all persons (empty datasets don't break anything)
const combinedPerson = fold(PersonMonoid, [
  fold(PersonMonoid, data1),
  fold(PersonMonoid, data2),
  fold(PersonMonoid, data3), // Empty dataset contributes identity element
  fold(PersonMonoid, data4)
]);
console.log("Combined person:", combinedPerson);

// 2. Safe age summation (works even with empty datasets)
const allAges = [
  ...data1.map(p => parseInt(p.age)),
  ...data2.map(p => parseInt(p.age)),
  ...data3.map(p => parseInt(p.age)), // Empty array - no problem!
  ...data4.map(p => parseInt(p.age))
];
const totalAge = fold(AgeSumMonoid, allAges);
console.log("Total age:", totalAge);

// 3. Safe name collection
const allNames = foldMap(
  NameListMonoid,
  (person: Person) => [person.name],
  [...data1, ...data2, ...data3, ...data4]
);
console.log("All names:", allNames);

// 4. Safe dataset merging with automatic empty handling
const allDatasets = fold(ArrayMonoid<Person>(), [data1, data2, data3, data4]);
console.log("Combined datasets:", allDatasets);
console.log("Total records:", allDatasets.length);

// 5. Comprehensive statistics with safe empty handling
const statsFromPerson = (p: Person): Stats => ({
  totalAge: parseInt(p.age),
  count: 1,
  names: [p.name],
  averageAge: parseInt(p.age)
});

const overallStats = foldMap(StatsMonoid, statsFromPerson, allDatasets);
console.log("Overall statistics:", overallStats);

// 6. Safe pipeline operations
const processDatasetSafely = (csvData: string): Stats => {
  const persons = readCSV(csvData);
  return foldMap(StatsMonoid, statsFromPerson, persons);
};

// These all work safely, even with problematic input
const results = [
  processDatasetSafely('name,age\nAlice,25'),
  processDatasetSafely(''), // Empty CSV
  processDatasetSafely('name,age'), // Header only
  processDatasetSafely('name,age\nBob,30\nCharlie,35')
].map(result => fold(StatsMonoid, [result]));

const finalResult = fold(StatsMonoid, results);
console.log("Pipeline result:", finalResult);

// 7. Error recovery with identity elements
const processWithRetry = (csvInputs: string[]): Stats => {
  const validResults: Stats[] = [];

  csvInputs.forEach(csv => {
    try {
      const result = processDatasetSafely(csv);
      validResults.push(result);
    } catch (error) {
      // Failed processing contributes identity element (safe)
      validResults.push(StatsMonoid.empty);
      console.log("Processing failed, using identity element");
    }
  });

  return fold(StatsMonoid, validResults);
};

const robustResult = processWithRetry([
  'name,age\nAlice,25',
  '', // Empty - safe
  'invalid,csv,format', // Might fail - safe
  'name,age\nBob,30'
]);

console.log("Robust processing result:", robustResult);
