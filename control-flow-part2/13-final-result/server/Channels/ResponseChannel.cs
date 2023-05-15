using System.Net;
using System.Text;
using System.Text.Json;

namespace Server.Channels;

public class ResponseChannel<T> : IFinalizeChannel<T>
{
    public async Task ProcessAsync(MessageContainer<T> message)
    {
        var context = message.Context;
        context.Response.ContentType = "application/json";
        
        if(message.Error == null)
        {
            context.Response.StatusCode = (int)HttpStatusCode.OK;
            await context.Response.WriteAsync(
                JsonSerializer.Serialize(message.Value),
                Encoding.UTF8);
        }
        else
        {
            context.Response.StatusCode = (int)message.Error.Value.Status;
            await context.Response.WriteAsync(
                JsonSerializer.Serialize(message.Error.Value.Errors),
                Encoding.UTF8);
        }
    }
}