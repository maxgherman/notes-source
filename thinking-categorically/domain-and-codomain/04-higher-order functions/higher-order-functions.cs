List<int> numbers = new List<int> { 1, 2, 3, 4, 5 };

// Select is map: Func<a, b> -> IEnumerable<a> -> IEnumerable<b>
var doubled = numbers.Select(x => x * 2);

// Where is filter: Func<a, bool> -> IEnumerable<a> -> IEnumerable<a>
var evens = numbers.Where(x => x % 2 == 0);

// Aggregate is reduce/fold
var sum = numbers.Aggregate(0, (acc, x) => acc + x);

// Higher-order function that returns a function
Func<int, Func<int, int>> CreateAdder = x => y => x + y;

var addTen = CreateAdder(10);
var result = addTen(5); // 15

Console.WriteLine(result);