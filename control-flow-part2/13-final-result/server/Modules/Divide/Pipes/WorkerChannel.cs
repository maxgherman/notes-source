using System.Net;
using Server.Channels;

namespace Server.Modules.Divide.Pipes;

public class WorkerChannel: IProcessChannel<DivideMessage, WorkerChannel.ExecutionResult>
{
    ServerChannel<WorkerChannel.ExecutionResult> serverChannel;
  
    public WorkerChannel(int bufferSize = 1000)
    {
        serverChannel = new ServerChannel<WorkerChannel.ExecutionResult>(bufferSize);
    }

    public async Task ProcessAsync(MessageContainer<DivideMessage> message)
    {
        if(message.IsPassThought)
        {
            await serverChannel.WriteAsync(new MessageContainer<WorkerChannel.ExecutionResult>(message.Context)
            {
                Error = message.Error,
                IsPassThought = true
            });

            return;
        }
        
        if(message.Value.Y == 0)
        {
            var error = new ProcessingError();
            error.Status = HttpStatusCode.BadRequest;
            error.Errors.Add( "Y: cannot be zero");
        
            await serverChannel.WriteAsync(new MessageContainer<WorkerChannel.ExecutionResult>(message.Context)
            {
                Error = error,
                IsPassThought = true
            });

            return;
        }
        
        await serverChannel.WriteAsync(new MessageContainer<WorkerChannel.ExecutionResult>(message.Context)
        {
            Value = new ExecutionResult
            {
                Value = message.Value.X / message.Value.Y
            },
            Error = null,
            IsPassThought = false
        });
    }

    public Task ReadAsync(Func<MessageContainer<WorkerChannel.ExecutionResult>, Task> getValue)
    {
        return serverChannel.ReadAsync(getValue);
    }

    public struct ExecutionResult
    {
        public decimal Value { get; set; }
    }
}