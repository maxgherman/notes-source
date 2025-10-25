// Func<TInput, TOutput> shows domain and codomain clearly
Func<string, int> parseNumber = input => int.Parse(input);
//   ^       ^
//   |       codomain (int)
//   domain (string)

Func<int, string> formatResult = value => $"Result: {value}";
//   ^    ^
//   |    codomain (string)
//   domain (int)

// Composition using method chaining
Func<string, string> processInput = input =>
    formatResult(parseNumber(input));
//   ^       ^
//   |       codomain (string)
//   domain (string)

// Example usage
Console.WriteLine(processInput("42")); // "Result: 42"