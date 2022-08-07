#nullable disable
// Comparison made to same variable
// We use these checks to quickly test for NaN.
#pragma warning disable CS1718 

using HelixToolkit.Wpf;

using Microsoft.FSharp.Core;

using System;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
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

    /// <summary>
    /// The <see cref="ModelVisual3D.Transform"/> of a ball is a <see cref="TranslateTransform3D"/>.
    /// </summary>
    ModelVisual3D[] balls;

    /// <summary>
    /// The <see cref="ModelVisual3D.Transform"/> of a moment arrow is a <see cref="Transform3DGroup"/>. <br />
    /// <see cref="Transform3DGroup.Children"/>[0] is a <see cref="RotateTransform3D"/>, where <see cref="RotateTransform3D.Rotation"/> is an <see cref="AxisAngleRotation3D"/>. <br />
    /// <see cref="Transform3DGroup.Children"/>[1] is a <see cref="TranslateTransform3D"/>.
    /// </summary>
    ModelVisual3D[] momentArrows;

    GeometryModel3D[] pathArrowModels;

    readonly ManualResetEventSlim unpauseEvent = new(initialState: true /* signaled */, spinCount: 0);
    
    readonly Color[] colors = typeof(Colors).GetProperties(BindingFlags.Public | BindingFlags.Static)
        .Where(a => a.PropertyType == typeof(Color))
        .Select(a => (Color)a.GetValue(null))
        .ToArray();

    static readonly Vector3 InitialPosition = new(-0.2, +0.3, -0.15);


    const double Radius = 0.01;
    const double Mass = 0.003;

    (Simulation.SimulatedObject, Simulation.SimulatedObject) objectPair = (
        new(InitialPosition, InitialPosition, Vector3.Zero, Vector3.Zero, initialMagneticMomentSmall, Mass * 10, Radius),
        new(Vector3.Zero, Vector3.Zero, Vector3.Zero, Vector3.Zero, initialMagneticMomentBig, Mass, Radius)
    );

    double periodMsec;

    static readonly Vector3 initialMagneticMomentSmall = new(0, 0, -1);
    static readonly Vector3 initialMagneticMomentBig = new(0, 0, 3);

    private void Create_Click(object sender, RoutedEventArgs e)
    {
        CreateResourcesButton.IsEnabled = false;
        
        var sphereBuilder = new MeshBuilder();
        sphereBuilder.AddSphere(new(0, 0, 0));

        var momentArrowBuilder = new MeshBuilder();
        momentArrowBuilder.AddArrow(new(0, 0, -3), new(0, 0, 3), 0.4);

        var pathArrowBuilder = new MeshBuilder();
        pathArrowBuilder.AddArrow(new(0, 0, -1), new(0, 0, 1), 0.2);

        var objects = (ITuple)objectPair;

        balls = new ModelVisual3D[objects.Length];
        momentArrows = new ModelVisual3D[objects.Length];
        pathArrowModels = new GeometryModel3D[objects.Length];

        for (int i = 0; i < objects.Length; i++)
        {
            var randomColor = colors[Random.Shared.Next(colors.Length)];

            static GeometryModel3D CreateFrozenModel(MeshBuilder builder, Brush brush)
            {
                brush.Freeze();
                Material material = new DiffuseMaterial(brush);
                material.Freeze();

                var mesh = builder.ToMesh(freeze: true);
                var model = new GeometryModel3D(mesh, material);
                model.Freeze();

                return model;
            }

            var sphereModel = CreateFrozenModel(sphereBuilder, new SolidColorBrush(randomColor) { Opacity = 0.75 });
            var momentArrowModel = CreateFrozenModel(momentArrowBuilder, new SolidColorBrush(Colors.Indigo));
            var pathArrowModel = CreateFrozenModel(pathArrowBuilder, new SolidColorBrush(randomColor.ChangeIntensity(0.8)) { Opacity = 0.25 });


            var visualRadius = 1; // ((Simulation.SimulatedObject)objects[i]).Radius * 100;
            balls[i] = new ModelVisual3D
            {
                Content = sphereModel,
                Transform = new Transform3DGroup
                {
                    Children = 
                    {
                        new ScaleTransform3D(new(visualRadius, visualRadius, visualRadius)),
                        new TranslateTransform3D()
                    }
                }
            };

            momentArrows[i] = new ModelVisual3D
            {
                Content = momentArrowModel,
                Transform = new Transform3DGroup
                {
                    Children =
                    {
                        new RotateTransform3D(new AxisAngleRotation3D()),
                        new TranslateTransform3D()
                    }
                }
            };

            pathArrowModels[i] = pathArrowModel;
        }
        
        foreach (var ball in balls)
        {
            Viewport.Children.Add(ball);
        }

        foreach (var smallMomentArrow in momentArrows)
        {
            Viewport.Children.Add(smallMomentArrow);
        }
    }

    private void Begin_Click(object sender, RoutedEventArgs e)
    {
        BeginSimulationButton.IsEnabled = false;
        void ThreadStart()
        {
            int counter = 0;
            Simulation.Calculate.runSimulation(ref objectPair,
                dt: 0.000001,
                momentOfInertia: o => (2.0 / 5) * o.Mass * o.Radius * o.Radius,
                gamma: i => 6 * Math.PI * Radius * 1.002e-3,
                callback: () =>
            {
                unpauseEvent.Wait();

                if (counter % 10 == 0)
                {
                    Dispatcher.InvokeAsync(() => UpdateUserInterface(ref counter), System.Windows.Threading.DispatcherPriority.Background);
                    if(!unpauseEvent.IsSet)
                    {
                        Console.WriteLine("FLYTHROUGH ERROR! PAUSE ISSUE!");
                    }
                }

                // If o.Position does not equal itself, o.Position is NaN.
                // At this point, the simulation is not giving us useful information, so stop simulating.
                return objectPair.Item1.Position == objectPair.Item1.Position &&
                       objectPair.Item2.Position == objectPair.Item2.Position;
            });
            MessageBox.Show("Simulation ended");
        }

        new Thread(ThreadStart)
        {
            IsBackground = true
        }.Start();

        PauseButton.IsEnabled = true;
    }

    private void UpdateUserInterface(ref int counter)
    {
        var objects = (ITuple)objectPair;
        for (int i = 0; i < objects.Length; i++)
        {
            var o = (Simulation.SimulatedObject)objects[i];
            var ballVisual = balls[i];
            var momentArrowVisual = momentArrows[i];

            var ballVisualTranslationGroup = (Transform3DGroup)ballVisual.Transform;
            var ballVisualTranslation= (TranslateTransform3D)ballVisualTranslationGroup.Children[1];

            var momentArrowVisualGroup = (Transform3DGroup)momentArrowVisual.Transform;
            var momentArrowVisualRotation = (RotateTransform3D)momentArrowVisualGroup.Children[0];
            var momentArrowVisualTranslation = (TranslateTransform3D)momentArrowVisualGroup.Children[1];

            var position = o.Position * 50;
            var positions = (position.X, position.Y, position.Z);

            (ballVisualTranslation.OffsetX, ballVisualTranslation.OffsetY, ballVisualTranslation.OffsetZ) = positions;
            (momentArrowVisualTranslation.OffsetX, momentArrowVisualTranslation.OffsetY, momentArrowVisualTranslation.OffsetZ) = positions;

            RotateToFaceDirection((AxisAngleRotation3D)momentArrowVisualRotation.Rotation, o.MagneticMoment);

            if (counter % 500 == 0)
            {
                var positionDelta = o.Position - o.PreviousPosition;
                if (positionDelta != positionDelta || positionDelta == Vector3.Zero)
                {
                    continue;
                }

                AddPathArrow(pathArrowModels[i], o, positions);
                //Console.WriteLine("Arrow made! #" + counter/500);
            }
        }

        counter++;

        void AddPathArrow(GeometryModel3D pathArrowModel, Simulation.SimulatedObject o, (double X, double Y, double Z) positions)
        {
            var axisAngleRotation = new AxisAngleRotation3D();
            RotateToFaceDirection(axisAngleRotation, o.Position - o.PreviousPosition);
            axisAngleRotation.Freeze();

            var pathArrowRotation = new RotateTransform3D(axisAngleRotation);
            pathArrowRotation.Freeze();

            var pathArrowTranslation = new TranslateTransform3D();
            (pathArrowTranslation.OffsetX, pathArrowTranslation.OffsetY, pathArrowTranslation.OffsetZ) = positions;
            pathArrowTranslation.Freeze();

            var transformGroup = new Transform3DGroup()
            {
                Children =
                {
                    pathArrowRotation,
                    pathArrowTranslation
                }
            };
            transformGroup.Freeze();

            Viewport.Children.Add(new ModelVisual3D()
            {
                Content = pathArrowModel,
                Transform = transformGroup
            });
        }
    }

    private static void RotateToFaceDirection(AxisAngleRotation3D arrowRotation, Vector3 direction)
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

    private void Pause_Click(object sender, RoutedEventArgs e)
    {
        if (unpauseEvent.IsSet)
        {
            unpauseEvent.Reset();
            PauseButton.Content = "Unpause";
            /*Console.WriteLine("Mass: " + objects[0].Mass + "\n" +
                              "Position: " + objects[0].Position + "\n" +
                              "Velocity: " + objects[0].Velocity + "\n" +
                              "Mag. Moment: " + objects[0].MagneticMoment + "\n" +
                              "Ang. Velocity: " + objects[0].AngularVelocity + "\n" +
                              "Simulation will NOT proceed as normal.\n"
            );*/
        }
        else
        {
            unpauseEvent.Set();
            PauseButton.Content = "Pause";
        }
    }
}
