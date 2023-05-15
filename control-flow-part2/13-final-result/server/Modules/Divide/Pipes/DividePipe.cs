using Server.Channels;

namespace Server.Modules.Divide.Pipes;

public class DividePipe
{
    private readonly ValidateChannel validateChannel;
    private readonly WorkerChannel workerChannel;
    private readonly ResponseChannel<WorkerChannel.ExecutionResult> responseChannel;

    private Func<HttpContext, DivideRequest, Task> pipeRunner;

    public DividePipe()
    {
        validateChannel = new ValidateChannel();
        workerChannel = new WorkerChannel();
        responseChannel = new ResponseChannel<WorkerChannel.ExecutionResult>();
        pipeRunner = new ChannelPipe()
            .Assemble(
                validateChannel,
                workerChannel,
                responseChannel
            ); 
    }

    public async Task RunAsync(HttpContext context, DivideRequest request)
    {
        await pipeRunner(context, request);
    }
}
