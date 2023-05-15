using System.Net;
using Server.Channels;

namespace Server.Modules.Divide.Pipes;

public class ValidateChannel : IProcessChannel<DivideRequest, DivideMessage>
{
    ServerChannel<DivideMessage> serverChannel;

    public ValidateChannel(int bufferSize = 1000)
    {
        serverChannel = new ServerChannel<DivideMessage>(bufferSize);
    }

    public async Task ProcessAsync(MessageContainer<DivideRequest> message)
    {
        var errors = new List<string>();
        
        if(!message.Value.X.HasValue)
        {
            errors.Add("X: no value provided");
        }

        if(!message.Value.Y.HasValue)
        {
            errors.Add("Y: no value provided");
        }

        if(errors.Any())
        {
            var error = new ProcessingError();
            error.Status = HttpStatusCode.BadRequest;
            error.Errors.AddRange(errors);
            
            await serverChannel.WriteAsync(new MessageContainer<DivideMessage>(message.Context)
            {
                IsPassThought = true,
                Error = error
            });    
        }
        else
        {
            await serverChannel.WriteAsync(new MessageContainer<DivideMessage>(message.Context)
            {
                IsPassThought = false,
                Value = new DivideMessage
                {
                    X = message.Value.X!.Value,
                    Y = message.Value.Y!.Value
                }
            });
        }
    }

    public Task ReadAsync(Func<MessageContainer<DivideMessage>, Task> getValue)
    {
        return serverChannel.ReadAsync(getValue);
    }
}