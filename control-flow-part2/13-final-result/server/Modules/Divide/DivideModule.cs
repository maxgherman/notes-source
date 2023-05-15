using Microsoft.AspNetCore.Mvc;
using Server.Modules.Divide.Pipes;

namespace Server.Modules.Divide;

public static class DivideModule
{
    public static IServiceCollection AddDivideServices(this IServiceCollection services)
    {
        services.AddSingleton<DividePipe>(new DividePipe());

        return services;
    }
    
    public static void MapDivideRoutes(this WebApplication app)
    {
        app.MapPost("/divide", async (
            DivideRequest request,
            HttpContext context,
            [FromServices] DividePipe pipe) =>
        {
            await pipe.RunAsync(context, request);
        });    
    }   
}