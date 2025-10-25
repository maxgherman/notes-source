// Partial function using nullable return type
static int? ParseInteger(string input)
{
    if (int.TryParse(input, out int result))
    {
        return result;
    }

    return null;  // Explicit representation of failure
}

// Usage with null checking
string userInput = "42";
int? parsed = ParseInteger(userInput);
if (parsed.HasValue)
{
    Console.WriteLine($"Result: {parsed.Value * 2}");
}
else
{
    Console.WriteLine("Invalid input");
}

// Domain: string (all strings)
// Codomain: int? (nullable integer)
