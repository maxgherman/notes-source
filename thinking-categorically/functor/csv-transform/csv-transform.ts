// data could be read from the file system
// const data = fs.readFileSync(filePath, 'utf8');

type Row = Record<string, string | number>;

function readCSV(data: string): Row[] {
  const [header, ...rows] = data.trim().split('\n');
  const keys = header.split(',');
  return rows.map<Row>((row) => {
    const values = row.split(',');
    // Initially all values are strings; fits Row (string | number)
    return Object.fromEntries(keys.map((k, i) => [k, values[i]])) as Row;
  });
}

// Functor: Array.prototype.map for transformation pipeline
const toNumberAge = (row: Row): Row => ({
  ...row,
  age: Number(row['age'] as string | number),
});

const addAgeGroup = (row: Row): Row => ({
  ...row,
  ageGroup: (row['age'] as number) > 30 ? 'old' : 'young',
});

const uppercaseName = (row: Row): Row => ({
  ...row,
  name: String(row['name']).toUpperCase(),
});

const pipeline: Array<(row: Row) => Row> = [
  toNumberAge,          // Convert age to number
  addAgeGroup,          // Add ageGroup
  uppercaseName,        // Uppercase name
];

// Apply pipeline to each row
function transformData(data: Row[], pipeline: Array<(row: Row) => Row>): Row[] {
  return data.map((row) => pipeline.reduce((acc, fn) => fn(acc), row));
}

// Example usage
const data: Row[] = readCSV('name,age\nAlice,25\nBob,40');
const transformed: Row[] = transformData(data, pipeline);

console.log(transformed);
