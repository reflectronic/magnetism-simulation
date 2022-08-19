using System.Numerics;

var initialPosition = new Vector3(-0.2f, +0.3f, -0.15f);

var initialMagneticMomentSmall = new Vector3(0, 0, -1);
var initialMagneticMomentBig = new Vector3(0, 0, 3);

const float Radius = 0.01f;
const float Mass = 0.003f;

var objectPair = (
    new Simulation.SimulatedObject(initialPosition, initialPosition, Vector3.Zero, Vector3.Zero, initialMagneticMomentSmall, Mass * 10, Radius),
    new Simulation.SimulatedObject(Vector3.Zero, Vector3.Zero, Vector3.Zero, Vector3.Zero, initialMagneticMomentBig, Mass, Radius)
);

int count = 0;

Simulation.Calculate.runSimulation(ref objectPair, 0.000001f, () =>
{
#if DEBUG
    count++;
    if (count % 10_000 == 0)
    {
        Console.WriteLine("Iteration " + count);
    }
#endif

    var (p1, p2) = (objectPair.Item1.Position, objectPair.Item2.Position);
#pragma warning disable CS1718 // Comparison made to same variable
    return p1 == p1 && p2 == p2;
#pragma warning restore CS1718
});

Console.WriteLine("Done simulation");