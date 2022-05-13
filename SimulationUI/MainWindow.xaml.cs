using HelixToolkit.Wpf;

using Simulation;

using System;
using System.Linq;
using System.Numerics;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Media3D;

namespace SimulationUI
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            var vectorField = Calculate.forceVectorField(
                Calculate.potentialField(
                    magneticMomentSmall: new(1, 1, 1),
                    magneticMomentBig: new(0, 0, 30_000),
                    sideLength: 16));

            var maxMagnitude = vectorField.OfType<Vector3>().MaxBy(v => v.Length()).Length();
            var minMagnitude = vectorField.OfType<Vector3>().MinBy(v => v.Length()).Length();


            for (int x = 0; x < vectorField.GetLength(0); x++)
            {
                for (int y = 0; y < vectorField.GetLength(1); y++)
                {
                    for (int z = 0; z < vectorField.GetLength(2); z++)
                    {
                        var direction = vectorField[x, y, z];
                        var originVector = new Vector3D(x, y, z) * 1.5;
                        var directionVector = new Vector3D(direction.X, direction.Y, direction.Z);
                        directionVector.Normalize();

                        var longerThanMin = direction.Length() - minMagnitude;
                        var rChannelColor = (byte)Math.Min(longerThanMin / (maxMagnitude - minMagnitude) * 255, 255);

                        var fill = new SolidColorBrush(Color.FromArgb(255, rChannelColor, 0, 0));
                        fill.Freeze();

                        var mat = new DiffuseMaterial(fill);
                        mat.Freeze();

                        var builder = new MeshBuilder();
                        builder.AddArrow((Point3D)originVector, (Point3D)(originVector + directionVector), 0.1, thetaDiv: 12);
                        var geometry = builder.ToMesh(true);

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

                        Viewport.Children.Add(visual);
                    }
                }
            }
        }
    }
}
