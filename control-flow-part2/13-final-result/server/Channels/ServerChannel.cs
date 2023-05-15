using System.Threading.Channels;

namespace Server.Channels;

public class ServerChannel<TOut>
{
    private readonly Channel<MessageContainer<TOut>> channel;
   
    public ServerChannel(int bufferSize = 1000)
    {
        channel = Channel.CreateBounded<MessageContainer<TOut>>(bufferSize);
    }

    public async Task WriteAsync(MessageContainer<TOut> message)
    {
        if(await channel.Writer.WaitToWriteAsync())
        {
            await channel.Writer.WriteAsync(message);
        }
    }

    public async Task ReadAsync(Func<MessageContainer<TOut>, Task> getValue)
    {
        var reader = channel.Reader;

        if(await reader.WaitToReadAsync())
        {
            if(reader.TryRead(out var message))
            {
                await getValue(message);
            }
        }
    }
}