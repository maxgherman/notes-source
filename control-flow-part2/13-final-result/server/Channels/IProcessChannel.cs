namespace Server.Channels;

public interface IProcessChannel<TIn, TOut>
{
    Task ProcessAsync(MessageContainer<TIn> value);
    
    Task ReadAsync(Func<MessageContainer<TOut>, Task> getValue);
}
