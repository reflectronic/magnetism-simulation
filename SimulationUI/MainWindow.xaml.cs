#nullable disable
using HelixToolkit.Wpf;

using System;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using System.Security.Cryptography.Xml;
using System.Threading;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Media3D;

using static Silk.NET.Maths.Vector3D;
using Vector3 = Silk.NET.Maths.Vector3D<double>;


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

    ModelVisual3D[] arrows;

    ModelVisual3D[] balls;
    ModelVisual3D cube;
    ModelVisual3D[] momentArrows;
    ModelVisual3D bigMomentArrow;
    const int sideLength = 15;

    Color[] colors = typeof(Colors).GetProperties(BindingFlags.Public | BindingFlags.Static)
        .Where(a => a.PropertyType == typeof(Color))
        .Select(a => (Color)a.GetValue(null))
        .ToArray();

    const double Mass = 0.003;
    const double Radius = 0.01;

    Simulation.Calculate.SimulatedObject[] objects = new Simulation.Calculate.SimulatedObject[]
    {
        new(new Vector3(-0.2f, 0.1f, 0.3f), Vector3.Zero, Vector3.Zero, initialMagneticMomentSmall, Mass),
    };

    double periodMsec;

    static readonly Vector3 initialMagneticMomentSmall = new(0, 0, -1);
    static readonly Vector3 initialMagneticMomentBig = new(0, 0, 1);


    static int XYZToIndex(int x, int y, int z)
    {
        const int zeroIndex = sideLength / 2;
        return (x + zeroIndex) * sideLength * sideLength + (y + zeroIndex) * sideLength + (z + zeroIndex);
    }

    const int LowBound = -sideLength / 2;
    const int HighBound = sideLength / 2;

    private void Calculate_Click(object sender, RoutedEventArgs e)
    {
        static ModelVisual3D CreateFrozenVisual(MeshBuilder builder, Brush brush, Transform3D transform)
        {
            brush.Freeze();
            var material = new DiffuseMaterial(brush);
            material.Freeze();

            var mesh = builder.ToMesh(freeze: true);
            var model = new GeometryModel3D(mesh, material);
            model.Freeze();

            return new ModelVisual3D
            {
                Content = model,
                Transform = transform
            };
        }

        var sphereBuilder = new MeshBuilder();
        sphereBuilder.AddSphere(new(0, 0, 0));

        balls = new ModelVisual3D[objects.Length];
        for (int i = 0; i < balls.Length; i++)
        {
            balls[i] = CreateFrozenVisual(sphereBuilder, new SolidColorBrush(colors[Random.Shared.Next(colors.Length)]) { Opacity = 0.75 }, new TranslateTransform3D());
        }

        var cubeBuilder = new MeshBuilder();
        cubeBuilder.AddBox(default(Point3D), 2, 2, 2);
        cube = CreateFrozenVisual(cubeBuilder, new SolidColorBrush(Colors.DarkSalmon) { Opacity = 0.5 }, null);
        

        var largeArrowBuilder = new MeshBuilder();
        largeArrowBuilder.AddArrow(new(0, 0, -3), new(0, 0, 3), 0.4);
        bigMomentArrow = CreateFrozenVisual(largeArrowBuilder, new SolidColorBrush(Colors.IndianRed), new RotateTransform3D(new AxisAngleRotation3D()));

        momentArrows = new ModelVisual3D[balls.Length];
        
        for (int i = 0; i < momentArrows.Length; i++)
        {
            momentArrows[i] = CreateFrozenVisual(largeArrowBuilder, new SolidColorBrush(Colors.Indigo), new Transform3DGroup
            {
                Children = 
                {
                    new RotateTransform3D(new AxisAngleRotation3D()),
                    new TranslateTransform3D()
                }
            });
        }

        var smallArrowBuilder = new MeshBuilder();
        smallArrowBuilder.AddArrow(new(0, 0, -0.25), new(0, 0, 0.25), 0.2, thetaDiv: 5);
        var arrowGeometry = smallArrowBuilder.ToMesh(freeze: true);

        /*arrows = new ModelVisual3D[sideLength * sideLength * sideLength];

        for (int x = LowBound; x <= HighBound; x++)
        {
            for (int y = LowBound; y <= HighBound; y++)
            {
                for (int z = LowBound; z <= HighBound; z++)
                {
                    var brush = new SolidColorBrush(Colors.Aquamarine) { Opacity = 0.7 };
                    brush.Freeze();

                    var material = new DiffuseMaterial(brush);
                    material.Freeze();

                    var model = new GeometryModel3D(arrowGeometry, material);
                    model.Freeze();

                    arrows[XYZToIndex(x, y, z)] = new ModelVisual3D
                    {
                        Content = model,
                        Transform = new Transform3DGroup()
                        {
                            Children =
                            {
                                new RotateTransform3D(new AxisAngleRotation3D()),
                                new TranslateTransform3D(x, y, z)
                            }
                        }
                    };
                }
            }
            
        }*/

        CalculateButton.IsEnabled = false;
    }

    private void Visualize_Click(object sender, RoutedEventArgs e)
    {
        foreach (var ball in balls)
        {
            Viewport.Children.Add(ball);
        }

        if (arrows != null)
        {
            foreach (var arrow in arrows)
            {
                Viewport.Children.Add(arrow);
            }
        }

        foreach (var smallMomentArrow in momentArrows)
            Viewport.Children.Add(smallMomentArrow);

        Viewport.Children.Add(cube);

        AxisAngleRotation3D arrowRotation = new();
        bigMomentArrow.Transform = new RotateTransform3D(arrowRotation);

        RotateArrowToFaceDirection(arrowRotation, initialMagneticMomentBig);

        VisualizeButton.IsEnabled = false;
    }

    private void Advance_Click(object sender, RoutedEventArgs e)
    {
        AdvanceButton.IsEnabled = false;

        void ThreadStart()
        {

            Simulation.Calculate.runSimulation(initialMagneticMomentBig, objects,
                dt: 0.00001f,
                momentOfInertia: i => (2.0 / 5) * objects[i].Mass * Radius * Radius,
                gamma: i => 6 * Math.PI * Radius * 1.002e-3,
                callback: () =>
            {

                //Thread.Sleep((int)periodMsec);
                void UpdateUserInterface()
                {
                    for (int i = 0; i < objects.Length; i++)
                    {
                        var ball = balls[i];
                        var arrow = momentArrows[i];
                        var translateBall = (TranslateTransform3D)ball.Transform;
                        var groupArrow = (Transform3DGroup)arrow.Transform;
                        var rotateArrow = (RotateTransform3D)groupArrow.Children[0];
                        var translateArrow = (TranslateTransform3D)groupArrow.Children[1];

                        var position = objects[i].Position * 50;

                        if (objects[0].Position.Length < 0.001)
                            MessageBox.Show("Collision");

                        var positions = (position.X, position.Y, position.Z);

                        (translateBall.OffsetX, translateBall.OffsetY, translateBall.OffsetZ) = positions;
                        (translateArrow.OffsetX, translateArrow.OffsetY, translateArrow.OffsetZ) = positions;

                        RotateArrowToFaceDirection((AxisAngleRotation3D)rotateArrow.Rotation, objects[0].MagneticMoment);
                    }

                    /*for (int x = LowBound; x <= HighBound; x++)
                    {
                        for (int y = LowBound; y <= HighBound; y++)
                        {
                            for (int z = LowBound; z <= HighBound; z++)
                            {
                                var position = new Vector3(x, y, z) / 50;
                                var vector = Simulation.Calculate.force(position, objects[0].MagneticMoment, initialMagneticMomentBig);

                                var visual = arrows[XYZToIndex(x, y, z)];

                                var group = (Transform3DGroup)visual.Transform;

                                var rotation = (RotateTransform3D)group.Children[0];
                                var quaternionRotation = (AxisAngleRotation3D)rotation.Rotation;

                                RotateArrowToFaceDirection(quaternionRotation, vector);
                            }
                        }
                    }*/
                }

                Dispatcher.InvokeAsync(UpdateUserInterface, System.Windows.Threading.DispatcherPriority.Background);
                return true;
            });
        }

        new Thread(ThreadStart)
        {
            IsBackground = true
        }.Start();
    }

    private static void RotateArrowToFaceDirection(AxisAngleRotation3D arrowRotation, Vector3 direction)
    {
        var directionA = Normalize(new Vector3(0, 0, 1));
        var directionB = Normalize(direction);

        var rotationAngle = Math.Acos(Dot(directionA, directionB));
        var rotationAxis = Cross(directionA, directionB);

        if (rotationAxis == Vector3.Zero)
        {
            // We ran into a special case. The two vectors could either be perpendicular or parallel.
            // We check the signs and rotate each component by 180 degrees if the signs of the components do not match.

            if (Math.Sign(directionA.X) != Math.Sign(directionB.X))
            {
                rotationAxis.Y = 1;
            }

            if (Math.Sign(directionA.Y) != Math.Sign(directionB.Y))
            {
                rotationAxis.Z = 1;
            }

            if (Math.Sign(directionA.Z) != Math.Sign(directionB.Z))
            {
                rotationAxis.X = 1;
            }
        }

        arrowRotation.Axis = rotationAxis.AsVector3D();
        arrowRotation.Angle = (180 / Math.PI) * rotationAngle;
    }

    private void Slider_Changed(object sender, RoutedPropertyChangedEventArgs<double> e)
    {
        periodMsec = e.NewValue;
    }
}
