namespace Server.Channels;

public interface IFinalizeChannel<T>
{
    Task ProcessAsync(MessageContainer<T> message);
}