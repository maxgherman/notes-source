// Reader
type Reader<R, A> = (env: R) => A;

class ReaderApplicative<R, A> {
  constructor(public readonly run: Reader<R, A>) {}

  static of<R, A>(value: A): ReaderApplicative<R, A> {
    return new ReaderApplicative(() => value);
  }

  map<B>(f: (a: A) => B): ReaderApplicative<R, B> {
    return new ReaderApplicative(env => f(this.run(env)));
  }

  ap<B>(this: ReaderApplicative<R, (a: B) => B>, fa: ReaderApplicative<R, B>): ReaderApplicative<R, B> {
    return new ReaderApplicative(env => this.run(env)(fa.run(env)));
  }
}

// Config
type Config = {
  ageThreshold: number;
  locale: 'en' | 'de';
};

// Parsed row
type Row = Record<string, string>;

// Transformed row
type TransformerRow = {
  original: Row;
  age?: number;
  ageGroup?: string;
  name?: string;
};

// Step 1: toNumber
const toNumber: ReaderApplicative<Config, (row: Row) => TransformerRow> =
  new ReaderApplicative(config => row => ({
    original: row,
    age: Number(row.age),
  }));

// Step 2: addAgeGroup
const addAgeGroup: ReaderApplicative<Config, (row: TransformerRow) => TransformerRow> =
  new ReaderApplicative(config => row => ({
    ...row,
    ageGroup:
      row.age == null
        ? 'young'
        : row.age > config.ageThreshold
        ? 'old'
        : 'young',
  }));

// Step 3: localizeName
const localizeName: ReaderApplicative<Config, (row: TransformerRow) => TransformerRow> =
  new ReaderApplicative(config => row => ({
    ...row,
    name:
      config.locale === 'de'
        ? row.original.name.toUpperCase()
        : row.original.name.toLowerCase(),
  }));

// ✅ Compose the transformations functionally
function transformDataWithReader(
  data: Row[],
  config: Config
): TransformerRow[] {

  // Compose the functions inside the Reader context
  const composed = toNumber.map(toNumFn =>
    addAgeGroup.map(addGroupFn =>
      localizeName.map(localizeFn => (row: Row): TransformerRow => {
        const step1 = toNumFn(row);
        const step2 = addGroupFn(step1);
        const step3 = localizeFn(step2);
        return step3;
      }).run(config)
    ).run(config)
  ).run(config);

  // Apply the composed function to each row
  return data.map(row => composed(row));
}

function readCSV(data: string): Row[] {
  const lines = data.trim().split(/\r?\n/);
  if (lines.length < 2) return [];

  const keys = lines[0].split(',').map(k => k.trim());

  return lines.slice(1).map(line => {
    const values = line.split(',').map(v => v.trim().replace(/^"|"$/g, ''));
    if (values.length !== keys.length) {
      throw new Error(`Malformed row: "${line}"`);
    }
    return Object.fromEntries(keys.map((k, i) => [k, values[i]]));
  });
}

const csvData = readCSV('name,age\nAlice,25\nBob,40');

const config: Config = {
  ageThreshold: 30,
  locale: 'de',
};

const transformed = transformDataWithReader(csvData, config);
console.log(transformed);
