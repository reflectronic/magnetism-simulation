using System.Windows.Media.Media3D;

namespace SimulationUI;
static class Extensions
{
    public static Vector3D AsVector3D(this Vectors.Vector3 vector3) => new(vector3.X, vector3.Y, vector3.Z);
}
