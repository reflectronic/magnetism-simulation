using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media.Media3D;

namespace SimulationUI;

static class Extensions
{
    public static Vector3D AsVector3D(this Silk.NET.Maths.Vector3D<double> vector3) => new(vector3.X, vector3.Y, vector3.Z);
}
