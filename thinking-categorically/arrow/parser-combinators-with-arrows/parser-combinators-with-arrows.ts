// Parser type mimicking Arrow behavior for parsing
interface Parser<A, B> {
  parse: (input: A) => B | null;
}

// Helper function to create parsers (similar to 'arr')
function parser<A, B>(f: (input: A) => B | null): Parser<A, B> {
  return { parse: f };
}

// Parser composition operator (similar to '>>>')
function composeParser<A, B, C>(
  first: Parser<A, B>,
  second: Parser<B, C>
): Parser<A, C> {
  return parser(input => {
    const intermediate = first.parse(input);
    if (intermediate === null) return null;
    return second.parse(intermediate);
  });
}

// Parallel parser composition for tuples
function parallelParser<A, B, C, D>(
  left: Parser<A, B>,
  right: Parser<C, D>
): Parser<[A, C], [B, D]> {
  return parser(([a, c]) => {
    const leftResult = left.parse(a);
    const rightResult = right.parse(c);
    if (leftResult === null || rightResult === null) return null;
    return [leftResult, rightResult];
  });
}

// Basic parsing primitives
const charParser = (target: string): Parser<string, string> =>
  parser(input => {
    if (input.length === 0) return null;
    const firstChar = input[0];
    return firstChar === target ? firstChar : null;
  });

const digitParser: Parser<string, number> =
  parser(char => {
    const code = char.charCodeAt(0);
    const isDigit = code >= 48 && code <= 57; // '0' to '9'
    return isDigit ? code - 48 : null;
  });

// Combined parsers using composition
const parseSpecificDigit = (digit: string): Parser<string, number> =>
  composeParser(charParser(digit), digitParser);

// Parse first character as digit
const parseFirstDigit: Parser<string, number> =
  parser(input => {
    if (input.length === 0) return null;
    const firstChar = input[0];
    const code = firstChar.charCodeAt(0);
    const isDigit = code >= 48 && code <= 57;
    return isDigit ? code - 48 : null;
  });

// String consumer that advances position
interface StringState {
  input: string;
  position: number;
}

// Stateful parser for consuming input
interface StatefulParser<A> {
  parse: (state: StringState) => { result: A; newState: StringState } | null;
}

function statefulParser<A>(f: (state: StringState) => { result: A; newState: StringState } | null): StatefulParser<A> {
  return { parse: f };
}

// Character parser that advances position
const charStatefulParser = (target: string): StatefulParser<string> =>
  statefulParser(state => {
    if (state.position >= state.input.length) return null;
    const char = state.input[state.position];
    if (char !== target) return null;
    return {
      result: char,
      newState: { input: state.input, position: state.position + 1 }
    };
  });

// Digit parser for stateful parsing
const digitStatefulParser: StatefulParser<number> =
  statefulParser(state => {
    if (state.position >= state.input.length) return null;
    const char = state.input[state.position];
    const code = char.charCodeAt(0);
    const isDigit = code >= 48 && code <= 57;
    if (!isDigit) return null;
    return {
      result: code - 48,
      newState: { input: state.input, position: state.position + 1 }
    };
  });

// Composition for stateful parsers
function composeStateful<A, B>(
  first: StatefulParser<A>,
  second: (a: A) => StatefulParser<B>
): StatefulParser<B> {
  return statefulParser(state => {
    const firstResult = first.parse(state);
    if (firstResult === null) return null;
    const secondParser = second(firstResult.result);
    return secondParser.parse(firstResult.newState);
  });
}

// Sequential composition (ignoring first result)
function thenStateful<A, B>(
  first: StatefulParser<A>,
  second: StatefulParser<B>
): StatefulParser<B> {
  return composeStateful(first, _ => second);
}

// Multi-digit number parser
const parseNumber: StatefulParser<number> =
  statefulParser(state => {
    let currentState = state;
    let digits: number[] = [];

    // Parse first digit (required)
    const firstResult = digitStatefulParser.parse(currentState);
    if (firstResult === null) return null;

    digits.push(firstResult.result);
    currentState = firstResult.newState;

    // Parse additional digits (optional)
    while (currentState.position < currentState.input.length) {
      const digitResult = digitStatefulParser.parse(currentState);
      if (digitResult === null) break;
      digits.push(digitResult.result);
      currentState = digitResult.newState;
    }

    // Convert digits array to number
    const result = digits.reduce((acc, digit) => acc * 10 + digit, 0);
    return { result, newState: currentState };
  });

// Alternative combinator (try first, then second)
function alternative<A>(
  first: StatefulParser<A>,
  second: StatefulParser<A>
): StatefulParser<A> {
  return statefulParser(state => {
    const firstResult = first.parse(state);
    if (firstResult !== null) return firstResult;
    return second.parse(state);
  });
}

// Parse identifier (letters followed by digits/letters)
const parseIdentifier: StatefulParser<string> =
  statefulParser(state => {
    let currentState = state;
    let chars: string[] = [];

    // First character must be a letter
    if (currentState.position >= currentState.input.length) return null;
    const firstChar = currentState.input[currentState.position];
    if (!/[a-zA-Z]/.test(firstChar)) return null;

    chars.push(firstChar);
    currentState = { input: currentState.input, position: currentState.position + 1 };

    // Subsequent characters can be letters or digits
    while (currentState.position < currentState.input.length) {
      const char = currentState.input[currentState.position];
      if (!/[a-zA-Z0-9]/.test(char)) break;
      chars.push(char);
      currentState = { input: currentState.input, position: currentState.position + 1 };
    }

    return { result: chars.join(''), newState: currentState };
  });

// Test functions
function testParsers() {
  console.log("Testing simple parsers:");
  console.log("parseFirstDigit('5abc'):", parseFirstDigit.parse("5abc")); // 5
  console.log("charParser('a')('abc'):", charParser('a').parse("abc")); // 'a'
  console.log("parseSpecificDigit('7')('7xyz'):", parseSpecificDigit('7').parse("7xyz")); // 7

  console.log("\nTesting stateful parsers:");
  const testState: StringState = { input: "123abc", position: 0 };
  console.log("parseNumber('123abc'):", parseNumber.parse(testState)); // { result: 123, newState: { input: "123abc", position: 3 } }

  const idState: StringState = { input: "hello123", position: 0 };
  console.log("parseIdentifier('hello123'):", parseIdentifier.parse(idState)); // { result: "hello123", newState: { input: "hello123", position: 8 } }

  // Test alternative parser with same types
  const numberOrId = alternative(
    parseNumber,
    statefulParser(state => {
      // Convert identifier result to number if it's all digits, otherwise return null
      const idResult = parseIdentifier.parse(state);
      if (idResult === null) return null;
      const num = parseInt(idResult.result, 10);
      return isNaN(num) ? null : { result: num, newState: idResult.newState };
    })
  );
  console.log("numberOrId('123abc'):", numberOrId.parse({ input: "123abc", position: 0 })); // number
  console.log("numberOrId('456def'):", numberOrId.parse({ input: "456def", position: 0 })); // number from identifier
}

// Advanced: Fluent parser builder
class ParserBuilder<A> {
  constructor(private _parser: StatefulParser<A>) {}

  then<B>(next: StatefulParser<B>): ParserBuilder<B> {
    return new ParserBuilder(thenStateful(this._parser, next));
  }

  map<B>(f: (a: A) => B): ParserBuilder<B> {
    return new ParserBuilder(statefulParser(state => {
      const result = this._parser.parse(state);
      if (result === null) return null;
      return { result: f(result.result), newState: result.newState };
    }));
  }

  or(alternativeParser: StatefulParser<A>): ParserBuilder<A> {
    return new ParserBuilder(alternative(this._parser, alternativeParser));
  }

  parse(input: string): A | null {
    const result = this._parser.parse({ input, position: 0 });
    return result ? result.result : null;
  }
}

// Factory function for fluent interface
function parseWith<A>(parser: StatefulParser<A>): ParserBuilder<A> {
  return new ParserBuilder(parser);
}

// Fluent interface example - corrected type handling
const numberParser = parseWith(parseNumber)
  .map(n => n * 2);

const stringParser = parseWith(parseIdentifier)
  .map(s => s.toUpperCase());

// For mixed types, we need to use union types
const mixedParser = parseWith(
  alternative(
    statefulParser(state => {
      const result = parseNumber.parse(state);
      return result ? { result: result.result as string | number, newState: result.newState } : null;
    }),
    statefulParser(state => {
      const result = parseIdentifier.parse(state);
      return result ? { result: result.result as string | number, newState: result.newState } : null;
    })
  )
);

console.log("Fluent parser test:");
console.log("numberParser('42abc'):", numberParser.parse("42abc")); // 84
console.log("stringParser('hello123'):", stringParser.parse("hello123")); // "HELLO123"
console.log("mixedParser('42abc'):", mixedParser.parse("42abc")); // 42
console.log("mixedParser('hello123'):", mixedParser.parse("hello123")); // "hello123"

// Run tests
testParsers();
