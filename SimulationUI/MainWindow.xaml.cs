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
        (Viewport.Camera as PerspectiveCamera)!.FieldOfView = 90;
    }

    ModelVisual3D[,,]? arrows;
    ModelVisual3D ball;
    ModelVisual3D smallMomentArrow;
    ModelVisual3D bigMomentArrow;

    Vector3 ballLocation = new(5, 5, 5);
    Vector3 magneticMomentSmall = new(0, 0, 1);
    Vector3 magneticMomentBig = new(0, 0, 1);
    int sideLength = 17, stepping = 1;

    private void Calculate_Click(object sender, RoutedEventArgs e)
    {
        static ModelVisual3D CreateFrozenVisual(MeshBuilder builder, Brush brush)
        {
            brush.Freeze();
            var material = new DiffuseMaterial(brush);
            material.Freeze();

            var mesh = builder.ToMesh(freeze: true);
            var model = new GeometryModel3D(mesh, material);
            model.Freeze();

            return new ModelVisual3D
            {
                Content = model
            };
        }

        var sphereBuilder = new MeshBuilder();
        sphereBuilder.AddSphere(new(0, 0, 0));
        ball = CreateFrozenVisual(sphereBuilder, new SolidColorBrush(Colors.CornflowerBlue) { Opacity = 0.5 });

        var largeArrowBuilder = new MeshBuilder();
        largeArrowBuilder.AddArrow(new(0, 0, -1), new(0, 0, 1), 0.4);
        smallMomentArrow = CreateFrozenVisual(largeArrowBuilder, new SolidColorBrush(Colors.Indigo));
        bigMomentArrow = CreateFrozenVisual(largeArrowBuilder, new SolidColorBrush(Colors.IndianRed));

        var smallArrowBuilder = new MeshBuilder();
        smallArrowBuilder.AddArrow(new(0, 0, -1), new(0, 0, 1), 0.2);
        var arrowGeometry = smallArrowBuilder.ToMesh(freeze: true);

        arrows = new ModelVisual3D[sideLength, sideLength, sideLength];

        for (int x = 0; x < sideLength; x++)
        {
            for (int y = 0; y < sideLength; y++)
            {
                for (int z = 0; z < sideLength; z++)
                {
                    // We'll share the arrow geometry for each of the arrows by applying a different transformation to each visual
                    arrows[x, y, z] = new ModelVisual3D
                    {
                        Content = new GeometryModel3D
                        {
                            Material = new DiffuseMaterial(new SolidColorBrush()),
                            Geometry = arrowGeometry
                        },
                        Transform = new Transform3DGroup()
                        { 
                            Children =
                            {
                                new RotateTransform3D(new AxisAngleRotation3D()),
                                new TranslateTransform3D()
                            }
                        }
                    };
                }
            }
        }

        CalculateButton.IsEnabled = false;
    }

    private void Visualize_Click(object sender, RoutedEventArgs e)
    {
        //Viewport.Children.Add(ball);

        if (arrows != null)
        {
            foreach (var visual in arrows)
            {
                Viewport.Children.Add(visual);
            }
        }

        VisualizeButton.IsEnabled = false;
    }

    private void Advance_Click(object sender, RoutedEventArgs e)
    {
        if (arrows is null)
        {
            MessageBox.Show("Please initialize the simualtion first.");
            return;
        }

        var vectorField = Calculate.simulateForces(sideLength: 17,
            stepping: 1, magneticMomentSmall,
            magneticMomentBig);

        var allRealLengths = vectorField.OfType<Vector3>().Select(v => v.Length()).Where(l => !float.IsNaN(l)).ToArray();
        Array.Sort(allRealLengths);

        var minMagnitude = allRealLengths[0];
        var maxMagnitude = allRealLengths[^1];

        var center = (vectorField.GetLength(0) - 1) / 2f * 4;
        Viewport.Children.Add(new CubeVisual3D { SideLength = 2,  Center = new Point3D(center, center, center), Material = new DiffuseMaterial(new SolidColorBrush(Colors.CornflowerBlue)) });


        Viewport.Children.Add(new CubeVisual3D { SideLength = 2, Center = new Point3D(10 * 4, 7 * 4, 9 * 4), Material = new DiffuseMaterial(new SolidColorBrush(Colors.HotPink) {  Opacity = .5 }) });


        for (int x = 0; x < vectorField.GetLength(0); x++)
        {
            for (int y = 0; y < vectorField.GetLength(1); y++)
            {
                for (int z = 0; z < vectorField.GetLength(2); z++)
                {
                    var vector = vectorField[x, y, z];

                    if (vector != vector)
                    {

                        continue;
                    }


                    var visual = arrows[x, y, z];

                    var model = (GeometryModel3D)visual.Content;
                    var material = (DiffuseMaterial)model.Material;

                    var longerThanMin = vector.Length() - minMagnitude;
                    var color = Color.FromArgb(255, (byte)Math.Min(longerThanMin / (maxMagnitude - minMagnitude) * 255, 255), 0, 0);

                    ((SolidColorBrush)material.Brush).Color = color;

                    var group = (Transform3DGroup)visual.Transform;

                    var rotation = (RotateTransform3D)group.Children[0];
                    var quaternionRotation = (AxisAngleRotation3D)rotation.Rotation;

                    var directionA = Vector3.Normalize(new Vector3(0, 0, 1));
                    var directionB = Vector3.Normalize(vector);

                    var rotationAngle = MathF.Acos(Vector3.Dot(directionA, directionB));
                    var rotationAxis = Vector3.Cross(directionA, directionB);

                    if (rotationAxis == Vector3.Zero)
                    {
                        // We ran into a special case. The two vectors could either be perpendicular or parallel.
                        // We check the signs and rotate each component by 180 degrees if the signs of the components do not match.

                        if (MathF.Sign(directionA.X) != MathF.Sign(directionB.X))
                        {
                            rotationAxis.Y = 1;
                        }

                        if (MathF.Sign(directionA.Y) != MathF.Sign(directionB.Y))
                        {
                            rotationAxis.Z = 1;
                        }

                        if (MathF.Sign(directionA.Z) != MathF.Sign(directionB.Z))
                        {
                            rotationAxis.X = 1;
                        }
                    }

                    quaternionRotation.Axis = rotationAxis.AsVector3D();
                    quaternionRotation.Angle = (180 / Math.PI) * rotationAngle;

                    /*
                    var r = vector.Length();
                    var theta = MathF.Atan2(vector.Z, r);
                    var phi = (x, y) switch
                    {
                        ( > 0, _) => MathF.Atan2(vector.Y, vector.X),
                        ( < 0, >= 0) => MathF.Atan2(vector.Y, vector.X) + MathF.PI,
                        ( < 0, < 0) => MathF.Atan2(vector.Y, vector.X) - MathF.PI,
                        (0, > 0) => MathF.PI / 2,
                        (0, < 0) => MathF.PI / -2,
                        (0, 0) => float.NaN
                    };

                    quaternionRotation.Quaternion = new(
                        x: -MathF.Sin(phi / 2) * MathF.Sin(theta / 2),
                        y: MathF.Cos(phi / 2) * MathF.Sin(theta / 2),
                        z: MathF.Sin(phi / 2) * MathF.Cos(theta / 2),
                        w: MathF.Cos(phi / 2) * MathF.Cos(theta / 2));*/

                    var translation = (TranslateTransform3D)group.Children[1];
                    translation.OffsetX = x * 4;
                    translation.OffsetY = y * 4;
                    translation.OffsetZ = z * 4;
                }
            }
        }

        AdvanceButton.IsEnabled = false;
    }
}
