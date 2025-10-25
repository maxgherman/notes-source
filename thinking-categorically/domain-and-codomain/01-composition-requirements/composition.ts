// Function type annotations show domain and codomain
const parseNumber: (input: string) => number = (input) => parseInt(input);
//                 ^               ^
//                 |               codomain (number)
//                 domain (string)

const formatResult: (value: number) => string = (value) => `Result: ${value}`;
//                  ^               ^
//                  |               codomain (string)
//                  domain (number)

// Function composition
const processInput: (input: string) => string = (input) =>
  formatResult(parseNumber(input));
//            ^          ^
//            |          domain matches codomain of parseNumber
//            codomain of parseNumber matches domain of formatResult

// Example usage
console.log(processInput("42")); // "Result: 42"