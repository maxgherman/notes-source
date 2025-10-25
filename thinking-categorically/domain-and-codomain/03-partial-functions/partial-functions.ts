// Partial function - might fail
function parseInteger(input: string): number | undefined {
  const result = parseInt(input);
  return isNaN(result) ? undefined : result;
}

// Usage with explicit null checking
const userInput = "42";
const parsed = parseInteger(userInput);
if (parsed !== undefined) {
  console.log(`Result: ${parsed * 2}`);
} else {
  console.log("Invalid input");
}

// Domain: string (all strings)
// Codomain: number | undefined (number or absence of value)