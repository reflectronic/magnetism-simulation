#nullable disable
// Comparison made to same variable
// We use these checks to quickly test for NaN.
#pragma warning disable CS1718 

using HelixToolkit.Wpf;

using System;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Media3D;

using Simulation;
using static Simulation.Vectors;
using Microsoft.FSharp.Core;

using ObjectPair = System.ValueTuple<Simulation.SimulatedObject, Simulation.SimulatedObject>;
using SimulationState = System.ValueTuple<bool, System.ValueTuple<Simulation.SimulatedObject, Simulation.SimulatedObject>>;

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

    GeometryModel3D[] pathArrowModels;

    ModelVisual3D externalFieldArrow;

    readonly ManualResetEventSlim unpauseEvent = new(initialState: true /* signaled */, spinCount: 0);
    
    readonly Color[] colors = typeof(Colors).GetProperties(BindingFlags.Public | BindingFlags.Static)
        .Where(a => a.PropertyType == typeof(Color))
        .Select(a => (Color)a.GetValue(null))
        .ToArray();

    bool engageThePerpendicularity = true;
    bool? textIsExpanding;

    (SimulatedObject, SimulatedObject) objectPair = Parameters.standardObjects();

    private void Create_Click(object sender, RoutedEventArgs e)
    {
        CreateResourcesButton.IsEnabled = false;
        
        var sphereBuilder = new MeshBuilder();
        sphereBuilder.AddSphere(new(0, 0, 0));

        var momentArrowBuilder = new MeshBuilder();
        momentArrowBuilder.AddArrow(new(0, 0, -3), new(0, 0, 3), 0.4);

        var momentArrowModel = CreateFrozenModel(momentArrowBuilder, new SolidColorBrush(Colors.Indigo));

        var pathArrowBuilder = new MeshBuilder();
        pathArrowBuilder.AddArrow(new(0, 0, -1), new(0, 0, 1), 0.2);

        var objects = (ITuple)objectPair;

        balls = new ModelVisual3D[objects.Length];
        pathArrowModels = new GeometryModel3D[objects.Length];

        for (int i = 0; i < objects.Length; i++)
        {
            var randomColor = colors[Random.Shared.Next(colors.Length)];

            var sphereModel = CreateFrozenModel(sphereBuilder, new SolidColorBrush(randomColor) { Opacity = 0.75 });
            var pathArrowModel = CreateFrozenModel(pathArrowBuilder, new SolidColorBrush(randomColor.ChangeIntensity(0.8)) { Opacity = 0.25 });

            var visualRadius = ((SimulatedObject)objects[i]).Radius * 1000;
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

            pathArrowModels[i] = pathArrowModel;
        }
        
        foreach (var ball in balls)
        {
            Viewport.Children.Add(ball);
        }

        Viewport.Children.Add(externalFieldArrow = new ModelVisual3D
        {
            Content = momentArrowModel,
            Transform = new RotateTransform3D(new AxisAngleRotation3D())
        });
    }

    private static GeometryModel3D CreateFrozenModel(MeshBuilder builder, Brush brush)
    {
        brush.Freeze();
        Material material = new DiffuseMaterial(brush);
        material.Freeze();

        var mesh = builder.ToMesh(freeze: true);
        var model = new GeometryModel3D(mesh, material);
        model.Freeze();

        return model;
    }

    private void Begin_Click(object sender, RoutedEventArgs e)
    {
        BeginSimulationButton.IsEnabled = false;
        void ThreadStart()
        {
            int counter = 0;
            Simulation.Simulation.run(objectPair,
                0.0001, // dt: 1 microsecond
                Vector3.Zero,
                true, // start the balls by expanding.
                (true, objectPair), // wasExpanding is true.
                callback: FuncConvert.FromFunc<(ObjectPair, bool, (FSharpOption<(Vector3, Vector3)>, FSharpOption<(Vector3, Vector3)>, SimulationState)), SimulationResult<SimulationState, ValueTuple>>((parameters) =>
            {
                unpauseEvent.Wait();


                var (objectPair, isExpanding, (c1, c2, (wasExpanding, lastPair))) = parameters;
                this.objectPair = objectPair;

                var externalField = Calculate.BFromPositions(objectPair, isExpanding, Parameters.fieldStrength(isExpanding));
                /*var perpField = new Vector3(0, 0, 1);

                var rotMat = default(Matrix3D);
                rotMat.Rotate(new Quaternion(new Vector3D(0, 1, 0), 20));
                var externalField = (perpField.AsVector3D() * rotMat).AsVector();*/

                if (counter % 5 == 0)
                {
                    
                    Dispatcher.InvokeAsync(() =>
                    {
                        UpdateUserInterface(externalField);
                        if (c1 != null)
                        {
                            var (p1, p2) = c1.Value;
                            AddCylinder(p1, p2, objectPair.Item1.Radius);
                        }

                        if (c2 != null)
                        {
                            var (p1, p2) = c2.Value;
                            AddCylinder(p1, p2, objectPair.Item2.Radius);
                        }

                    }, System.Windows.Threading.DispatcherPriority.Background);
                }

                engageThePerpendicularity = isExpanding;

                counter++;

                void Log(string message) => Dispatcher.Invoke(() => LogItems.Items.Add($"{counter}: {message}"), System.Windows.Threading.DispatcherPriority.SystemIdle);

                if (isExpanding != wasExpanding)
                {
                    //Log($"Previous simulation was {(wasExpanding ? "" : "not ")}expanding, next simulation will be {(isExpanding ? "" : "not ")}expanding.");
                }   

                if (Length(lastPair.Item1.Position - lastPair.Item2.Position) == Length(objectPair.Item1.Position - objectPair.Item2.Position))
                {
                   Log("Objects have gotten further away.");
                }

                // If o.Position does not equal itself, o.Position is NaN.  
                // At this point, the simulation is not giving us useful information, so stop simulating.
                return objectPair != objectPair
                    ? SimulationResult<SimulationState, ValueTuple>.NewEndSimulation(default)
                    : SimulationResult<SimulationState, ValueTuple>.NewContinueSimulation(externalField, (isExpanding, objectPair));
            }));

            MessageBox.Show("Simulation ended");
        }

        new Thread(ThreadStart)
        {
            IsBackground = true
        }.Start();

        PauseButton.IsEnabled = true;
    }

    private void UpdateUserInterface(Vector3 externalField)
    {
        var objects = (ITuple)objectPair;
        for (int i = 0; i < objects.Length; i++)
        {
            var o = (SimulatedObject)objects[i];
            var ballVisual = balls[i];

            var ballVisualTranslationGroup = (Transform3DGroup)ballVisual.Transform;
            var ballVisualTranslation = (TranslateTransform3D)ballVisualTranslationGroup.Children[1];

            var position = o.Position * 1000;
            var positions = (position.X, position.Y, position.Z);

            // var newPt = o.Path.Head;
            // var oldPt = o.Path.Tail.Head;
            // AddCylinder(newPt.Position, oldPt.Position, newPt.Radius);

            (ballVisualTranslation.OffsetX, ballVisualTranslation.OffsetY, ballVisualTranslation.OffsetZ) = positions;
        }

        RotateToFaceDirection((AxisAngleRotation3D)((RotateTransform3D)externalFieldArrow.Transform).Rotation, externalField);

        if (textIsExpanding != engageThePerpendicularity)
        {
            ExpandingOrContracting.Dispatcher.Invoke(() =>
            {
                ExpandingOrContracting.Text = engageThePerpendicularity ? "Expanding" : "Contracting";
            });

            textIsExpanding = engageThePerpendicularity;
        }

        static void RotateToFaceDirection(AxisAngleRotation3D arrowRotation, Vector3 direction)
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
                    rotationAxis = new Vector3(rotationAxis.X, 1, rotationAxis.Z);
                }

                if (Math.Sign(directionA.Y) != Math.Sign(directionB.Y))
                {
                    rotationAxis = new Vector3(rotationAxis.X, rotationAxis.Y, 1);
                }

                if (Math.Sign(directionA.Z) != Math.Sign(directionB.Z))
                {
                    rotationAxis = new Vector3(1, rotationAxis.Y, rotationAxis.Z);
                }
            }

            arrowRotation.Axis = rotationAxis.AsVector3D();
            arrowRotation.Angle = (180 / Math.PI) * rotationAngle;
        }
    }

    private void AddCylinder(Vector3 pt1, Vector3 pt2, double radius)
    {
        var builder = new MeshBuilder();
        builder.AddCylinder((pt1.AsVector3D() * 1000).ToPoint3D(), (pt2.AsVector3D() * 1000).ToPoint3D(), radius * 1000);
        var model = CreateFrozenModel(builder, new SolidColorBrush(Colors.GreenYellow));
        Viewport.Children.Add(new ModelVisual3D { Content = model });
    }

    private void Pause_Click(object sender, RoutedEventArgs e)
    {
        if (unpauseEvent.IsSet)
        {
            unpauseEvent.Reset();
            PauseButton.Content = "Unpause";
        }
        else
        {
            unpauseEvent.Set();
            PauseButton.Content = "Pause";
        }
    }

    private void Flip_Click(object sender, RoutedEventArgs e)
    {
        engageThePerpendicularity = !engageThePerpendicularity;
    }
}
