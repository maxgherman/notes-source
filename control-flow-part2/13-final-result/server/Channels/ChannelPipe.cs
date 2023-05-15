namespace Server.Channels;

public class ChannelPipe
{
    public Func<HttpContext, T1, Task> Assemble<T1, T2, T3>(
        IProcessChannel<T1, T2> channel1,
        IProcessChannel<T2, T3> channel2,
        IFinalizeChannel<T3> finalizeChannel)
    {
        return async (context, request) => {
            await channel1.ProcessAsync(new MessageContainer<T1>(context)
            {
                Value = request
            });

            await channel1.ReadAsync(channel2.ProcessAsync);
            await channel2.ReadAsync(finalizeChannel.ProcessAsync);
        };
    }
}