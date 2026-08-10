// C# example with protocol message routing
using System;

public interface IMessageHandler<in T>
{
    void Handle(T message);
}

// Implementation of the routing handler (for 'choose' operation)
public class RoutingHandler<T, B, C> : IMessageHandler<T>
{
    private readonly Func<T, Either<B, C>> discriminate;
    private readonly IMessageHandler<B> handlerB;
    private readonly IMessageHandler<C> handlerC;

    public RoutingHandler(
        Func<T, Either<B, C>> discriminate,
        IMessageHandler<B> handlerB,
        IMessageHandler<C> handlerC)
    {
        this.discriminate = discriminate;
        this.handlerB = handlerB;
        this.handlerC = handlerC;
    }

    public void Handle(T message)
    {
        var result = discriminate(message);
        if (result.IsLeft)
        {
            handlerB.Handle(result.Left);
        }
        else
        {
            handlerC.Handle(result.Right);
        }
    }
}

// Decidable-like operations
public static class MessageRouter
{
    public static IMessageHandler<T> Choose<T, B, C>(
        Func<T, Either<B, C>> discriminate,
        IMessageHandler<B> handlerB,
        IMessageHandler<C> handlerC) =>
        new RoutingHandler<T, B, C>(discriminate, handlerB, handlerC);
}

public class Either<TLeft, TRight>
{
    public bool IsLeft { get; }
    public TLeft Left { get; }
    public TRight Right { get; }

    private Either(bool isLeft, TLeft left, TRight right)
    {
        IsLeft = isLeft;
        Left = left;
        Right = right;
    }

    public static Either<TLeft, TRight> NewLeft(TLeft value) =>
        new(true, value, default!);

    public static Either<TLeft, TRight> NewRight(TRight value) =>
        new(false, default!, value);
}

// Protocol messages
public record NetworkMessage;
public record HttpRequest(string Url, string Method) : NetworkMessage;
public record WebSocketMessage(string Data) : NetworkMessage;

// Specific handlers
public class HttpHandler : IMessageHandler<HttpRequest>
{
    public void Handle(HttpRequest request) =>
        Console.WriteLine($"HTTP {request.Method} {request.Url}");
}

public class WebSocketHandler : IMessageHandler<WebSocketMessage>
{
    public void Handle(WebSocketMessage message) =>
        Console.WriteLine($"WebSocket: {message.Data}");
}

// Route messages based on protocol type
public class NetworkMessageHandler : IMessageHandler<NetworkMessage>
{
    private readonly IMessageHandler<NetworkMessage> router;

    public NetworkMessageHandler()
    {
        router = MessageRouter.Choose<NetworkMessage, HttpRequest, WebSocketMessage>(
            message => message switch
            {
                HttpRequest req => Either<HttpRequest, WebSocketMessage>.NewLeft(req),
                WebSocketMessage ws => Either<HttpRequest, WebSocketMessage>.NewRight(ws),
                _ => throw new ArgumentException("Unknown message type")
            },
            new HttpHandler(),
            new WebSocketHandler()
        );
    }

    public void Handle(NetworkMessage message) => router.Handle(message);
}

// Example usage and main program
public class Program
{
    public static void Main(string[] args)
    {
        Console.WriteLine("=== C# Decidable Pattern: Protocol Message Routing ===\n");

        // Create the main message handler
        var messageHandler = new NetworkMessageHandler();

        // Test messages
        var testMessages = new NetworkMessage[]
        {
            new HttpRequest("https://api.example.com/users", "GET"),
            new WebSocketMessage("Hello from WebSocket!"),
            new HttpRequest("https://api.example.com/orders", "POST"),
            new WebSocketMessage("Real-time update: Order #123 shipped"),
            new HttpRequest("https://api.example.com/products/42", "PUT")
        };

        Console.WriteLine("Processing messages through Decidable router:\n");

        // Process each message - the router will automatically discriminate
        // and route to the appropriate handler
        foreach (var message in testMessages)
        {
            Console.Write($"Message: {message.GetType().Name} -> ");
            messageHandler.Handle(message);
        }

        Console.WriteLine("\n=== Advanced Example: Multi-level Routing ===\n");

        // Create a more sophisticated router that handles errors
        var advancedHandler = CreateAdvancedHandler();

        var advancedMessages = new object[]
        {
            new HttpRequest("https://secure.api.com/auth", "POST"),
            new WebSocketMessage("User connected"),
            "Invalid message type",  // This will route to error handler
            new HttpRequest("https://api.com/data", "GET")
        };

        foreach (var message in advancedMessages)
        {
            Console.Write($"Processing: {message.GetType().Name} -> ");
            try
            {
                advancedHandler.Handle(message);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
            }
        }

        // C# has no ordinary, non-null uninhabited type. A general 'lose'
        // operation therefore cannot be implemented safely without adding a
        // convention such as throwing an exception. The Haskell examples use
        // Void, where the impossibility is represented by the type system.
    }

    // Advanced handler that includes error handling
    private static IMessageHandler<object> CreateAdvancedHandler()
    {
        return MessageRouter.Choose<object, NetworkMessage, string>(
            input => input switch
            {
                NetworkMessage netMsg => Either<NetworkMessage, string>.NewLeft(netMsg),
                string str => Either<NetworkMessage, string>.NewRight(str),
                _ => Either<NetworkMessage, string>.NewRight($"Unknown type: {input.GetType().Name}")
            },
            new NetworkMessageHandler(),  // Handle valid network messages
            new ErrorHandler()            // Handle errors/invalid messages
        );
    }
}

// Error handler for invalid message types
public class ErrorHandler : IMessageHandler<string>
{
    public void Handle(string error)
    {
        Console.WriteLine($"ERROR: {error}");
    }
}
