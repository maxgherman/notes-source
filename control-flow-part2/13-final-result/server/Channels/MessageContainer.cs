using System.Net;

namespace Server.Channels;

public class MessageContainer<T>
{
    public MessageContainer(
        HttpContext context)
    {
        Context = context;
        IsPassThought = false;
        Error = null;
    }

    public bool IsPassThought { get; set; }
    
    public T? Value { get; set; }

    public ProcessingError? Error { get; set;}

    public HttpContext Context { get; private set;}
}

public struct ProcessingError
{
    public HttpStatusCode Status {get; set;}
    
    public List<string> Errors { get; set; }

    public ProcessingError()
    {
        Status = HttpStatusCode.Processing;
        Errors = new List<string>();
    }
}