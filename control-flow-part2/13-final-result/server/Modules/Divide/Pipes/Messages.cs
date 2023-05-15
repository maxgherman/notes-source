namespace Server.Modules.Divide.Pipes;

public struct DivideRequest
{
    public decimal? X { get; set; }

    public decimal? Y { get; set; }
}

public struct DivideMessage
{
    public decimal X { get; set; }

    public decimal Y { get; set; }
}