using HelixToolkit.Wpf;

using Simulation;

using System;
using System.Diagnostics;
using System.Linq;
using System.Numerics;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Media3D;

namespace SimulationUI;

/// <summary>
/// Interaction logic for MainWindow.xaml
/// </summary>
public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();
        (Viewport.Camera as PerspectiveCamera)!.FieldOfView = 70;
    }

    private void Button_Click(object sender, RoutedEventArgs e)
    {
        var ballLoc = new Vector3(5, 5, 5);
        var magneticMomentSmall = new Vector3(0, 0, 1);
        var magneticMomentBig = new Vector3(0, 0, 1);


        var vectorField = Calculate.simulateForces(sideLength: 17,
            stepping: 1,
            magneticMomentSmall,
            magneticMomentBig);

        var vectors = vectorField.OfType<Vector3>().Where(v => !double.IsNaN(v.Length()));

        var normalized = Vector3.Normalize(magneticMomentSmall);
        var smallMomentArrow = CreateVisual(
            builder => builder.AddArrow(
                (Point3D)GetOriginFromXYZ(ballLoc).AsVector3D(),
                (Point3D)(GetOriginFromXYZ(ballLoc) + normalized * 4).AsVector3D(),
                0.4), 
            () => new SolidColorBrush(Colors.PaleGreen));
        Viewport.Children.Add(smallMomentArrow);

        var center = new Vector3(vectorField.GetLength(0) / 2);

        var bigMomentArrow = CreateVisual(
            builder => builder.AddArrow(
                (Point3D)GetOriginFromXYZ(center).AsVector3D(),
                (Point3D)GetOriginFromXYZ(center + Vector3.Normalize(magneticMomentBig) * 2).AsVector3D(),
                0.4),
            () => new SolidColorBrush(Colors.Honeydew));
        Viewport.Children.Add(bigMomentArrow);

        var maxMagnitude = vectors.Select(v => v.Length()).Max();
        var minMagnitude = vectors.Select(v => v.Length()).Min();

        for (int x = 0; x < vectorField.GetLength(0); x++)
        {
            for (int y = 0; y < vectorField.GetLength(1); y++)
            {
                for (int z = 0; z < vectorField.GetLength(2); z++)
                {
                    var direction = vectorField[x, y, z];
                    Vector3D originPositionVector = GetOriginFromXYZ(new(x, y, z)).AsVector3D();
                    var directionVector = direction.AsVector3D();
                    directionVector.Normalize();

                    var longerThanMin = direction.Length() - minMagnitude;
                    var color = Color.FromArgb(255, (byte)Math.Min(longerThanMin / (maxMagnitude - minMagnitude) * 255, 255), 0, 0);

                    var visual = CreateVisual(builder => builder.AddArrow((Point3D)originPositionVector, (Point3D)(originPositionVector + directionVector), diameter: 0.1, thetaDiv: 12), () => new SolidColorBrush(color));

                    Viewport.Children.Add(visual);

                    if (x == 10 && y == 8 && z == 8)
                    {
                        var otherPos = originPositionVector;
                        otherPos.Z += 0.5;
                        Viewport.Children.Add(CreateVisual(b => b.AddCylinder((Point3D)originPositionVector, (Point3D)otherPos, 0.4), () => new SolidColorBrush(Colors.HotPink)));
                    }

                    if (x == y && y == z && x == vectorField.GetLength(0) / 2)
                    {
                        var cube = CreateVisual(builder => builder.AddBox((Point3D)originPositionVector, 1, 1, 1), () => new SolidColorBrush(Colors.CornflowerBlue));
                        Viewport.Children.Add(cube);
                    }
                }
            }
        }

        var sphere = CreateVisual(builder => builder.AddSphere((Point3D)GetOriginFromXYZ(ballLoc).AsVector3D()), () => new SolidColorBrush(Colors.IndianRed) { Opacity = 0.5 });
        Viewport.Children.Add(sphere);

        static ModelVisual3D CreateVisual(Action<MeshBuilder> function, Func<Brush> brush)
        {
            var fill = brush();
            fill.Freeze();

            var mat = new DiffuseMaterial(fill);
            mat.Freeze();

            var builder = new MeshBuilder();
            function(builder);
            var geometry = builder.ToMesh(freeze: true);

            var model = new GeometryModel3D
            {
                Material = mat,
                Geometry = geometry
            };
            model.Freeze();

            var visual = new ModelVisual3D
            {
                Content = model
            };

            return visual;
        }

        static Vector3 GetOriginFromXYZ(Vector3 array)
        {
            var vector = array * 2;
            vector.Y *= 2;
            return vector;
        }
    }
}
