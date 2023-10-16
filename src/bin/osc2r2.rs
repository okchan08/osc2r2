use osc2r2;

use bevy::{
    input::mouse::{MouseMotion, MouseWheel},
    pbr::wireframe::{Wireframe, WireframeConfig, WireframePlugin},
    prelude::{shape::Box, *},
    render::render_resource::{PrimitiveTopology, WgpuFeatures},
    render::settings::WgpuSettings,
    render::{mesh, RenderPlugin},
};

#[derive(Resource)]
struct BevyOpenDriveWrapper(osc2r2::open_drive::open_drive::OpenDrive);

fn main() {
    let odr = BevyOpenDriveWrapper(osc2r2::open_drive::open_drive::OpenDrive::parse_open_drive(
        "./Town04.xodr",
    ));

    App::new()
        .insert_resource(ClearColor(Color::DARK_GRAY))
        .insert_resource(odr)
        .add_plugins((
            DefaultPlugins.set(RenderPlugin {
                wgpu_settings: WgpuSettings {
                    features: WgpuFeatures::POLYGON_MODE_LINE,
                    ..default()
                },
            }),
            WireframePlugin,
        ))
        .add_systems(Startup, setup)
        .add_systems(Update, (pan_orbit_camera, update_actor))
        .run();
}

#[derive(Component)]
struct PanOrbitCamera {
    pub focus: Vec3,
    pub radius: f32,
    pub upside_down: bool,
}

impl Default for PanOrbitCamera {
    fn default() -> Self {
        Self {
            focus: Vec3::ZERO,
            radius: 5.0,
            upside_down: false,
        }
    }
}

fn pan_orbit_camera(
    windows: Query<&Window>,
    input_mouse: Res<Input<MouseButton>>,
    mut ev_motion: EventReader<MouseMotion>,
    mut ev_scroll: EventReader<MouseWheel>,
    mut query: Query<(&mut PanOrbitCamera, &mut Transform, &Projection), With<Camera>>,
) {
    let orbit_button = MouseButton::Right;
    let pan_button = MouseButton::Left;

    let mut pan = Vec2::ZERO;
    let mut rotation_move = Vec2::ZERO;
    let mut scroll = 0.0;
    let mut orbit_button_changed = false;

    if input_mouse.pressed(orbit_button) {
        for ev in ev_motion.iter() {
            rotation_move += ev.delta / 2.0;
        }
    } else if input_mouse.pressed(pan_button) {
        for ev in ev_motion.iter() {
            pan += ev.delta / 2.0;
        }
    }

    for ev in ev_scroll.iter() {
        scroll += ev.y / 10.0;
    }

    if input_mouse.just_released(orbit_button) || input_mouse.just_pressed(orbit_button) {
        orbit_button_changed = true;
    }

    for (mut pan_orbit, mut transform, projection) in query.iter_mut() {
        if orbit_button_changed {
            // only check for upside down when orbiting started or ended this frame
            // if the camera is "upside" down, panning horizontally would be inverted, so invert the input to make it correct
            let up = transform.rotation * Vec3::Y;
            pan_orbit.upside_down = up.y <= 0.0;
        }

        let mut any = false;
        if rotation_move.length_squared() > 0.0 {
            any = true;
            let window = get_primary_window_size(windows.single());
            let delta_x = {
                let delta = rotation_move.x / window.x * std::f32::consts::PI * 2.0;
                if pan_orbit.upside_down {
                    -delta
                } else {
                    delta
                }
            };
            let delta_y = rotation_move.y / window.y * std::f32::consts::PI;
            let pitch = Quat::from_rotation_x(-delta_x);
            let yaw = Quat::from_rotation_y(-delta_y);
            transform.rotation = yaw * transform.rotation; // rotate around global y axis
            transform.rotation = pitch * transform.rotation; // rotate around global x axis
        } else if pan.length_squared() > 0.0 {
            any = true;
            // make panning distance independent of resolution and FOV,
            let window = get_primary_window_size(windows.single());
            if let Projection::Perspective(projection) = projection {
                pan *= Vec2::new(projection.fov * projection.aspect_ratio, projection.fov) / window;
            }
            // translate by local axes
            let right = transform.rotation * Vec3::X * -pan.x;
            let up = transform.rotation * Vec3::Y * pan.y;
            // make panning proportional to distance away from focus point
            let translation = (right + up) * pan_orbit.radius;
            pan_orbit.focus += translation;
        } else if scroll.abs() > 0.0 {
            any = true;
            pan_orbit.radius -= scroll * pan_orbit.radius * 0.2;
            // dont allow zoom to reach zero or you get stuck
            pan_orbit.radius = f32::max(pan_orbit.radius, 0.05);
        }
        if any {
            // emulating parent/child to make the yaw/y-axis rotation behave like a turntable
            // parent = x and y rotation
            // child = z-offset
            let rot_matrix = Mat3::from_quat(transform.rotation);
            transform.translation =
                pan_orbit.focus + rot_matrix.mul_vec3(Vec3::new(0.0, 0.0, pan_orbit.radius));
        }
    }
    // consume any remaining events, so they don't pile up if we don't need them
    // (and also to avoid Bevy warning us about not checking events every frame update)
    ev_motion.clear();
}

fn get_primary_window_size(window: &Window) -> Vec2 {
    Vec2::new(window.width(), window.height())
}

#[derive(Component)]
struct Actor {
    pub road_id: String,
    pub lane_id: i32,
    pub s: f64,
    pub height: f32,
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    odr: Res<BevyOpenDriveWrapper>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut wireframe_config: ResMut<WireframeConfig>,
) {
    let road_mesh = odr.0.get_road_network_mesh(0.1);
    let to_debug = false;

    if to_debug {
        wireframe_config.global = true;
        println!(
            "#vtx: {:?}   #idx: {:?}",
            road_mesh.lane_mesh.mesh.vertices.len(),
            road_mesh.lane_mesh.mesh.indicies.len()
        );

        println!("{:?}", road_mesh.lane_mesh.mesh.indicies);

        for i in (0..road_mesh.lane_mesh.mesh.indicies.len()).step_by(3) {
            let i0 = road_mesh.lane_mesh.mesh.indicies.get(i).unwrap();
            let i1 = road_mesh.lane_mesh.mesh.indicies.get(i + 1).unwrap();
            let i2 = road_mesh.lane_mesh.mesh.indicies.get(i + 2).unwrap();
            let pos0 = road_mesh.lane_mesh.mesh.vertices.get(*i0 as usize).unwrap();
            let pos1 = road_mesh.lane_mesh.mesh.vertices.get(*i1 as usize).unwrap();
            let pos2 = road_mesh.lane_mesh.mesh.vertices.get(*i2 as usize).unwrap();
            println!("{:?}  {:?}  {:?}", pos0, pos1, pos2);
        }
    }

    let translation = Vec3::new(500.0, 25.0, 50.0);
    let radius = translation.length();

    commands.spawn((
        Camera3dBundle {
            camera: Camera {
                hdr: true, // 1. HDR is required for bloom
                ..default()
            },
            transform: Transform::from_translation(translation).looking_at(Vec3::ZERO, Vec3::Z),
            ..default()
        },
        PanOrbitCamera {
            radius: radius,
            ..default()
        },
    ));

    let mut mesh = Mesh::new(PrimitiveTopology::TriangleList);

    mesh.insert_attribute(
        Mesh::ATTRIBUTE_POSITION,
        road_mesh.lane_mesh.mesh.get_bevy_mesh_position(0.0),
    );
    mesh.insert_attribute(
        Mesh::ATTRIBUTE_NORMAL,
        road_mesh.lane_mesh.mesh.get_bevy_mesh_normals(),
    );
    mesh.insert_attribute(
        Mesh::ATTRIBUTE_UV_0,
        road_mesh.lane_mesh.mesh.get_bevy_mesh_uv(),
    );
    mesh.set_indices(Some(mesh::Indices::U32(
        road_mesh.lane_mesh.mesh.indicies.clone(),
    )));

    commands.spawn((
        PbrBundle {
            mesh: meshes.add(mesh),
            material: materials.add(Color::rgba(0.2, 0.2, 0.2, 1.0).into()),
            ..default()
        },
        //Wireframe,
    ));

    let mut roadmark_mesh = Mesh::new(PrimitiveTopology::TriangleList);
    roadmark_mesh.insert_attribute(
        Mesh::ATTRIBUTE_POSITION,
        road_mesh.road_mark_mesh.mesh.get_bevy_mesh_position(0.01),
    );
    roadmark_mesh.insert_attribute(
        Mesh::ATTRIBUTE_NORMAL,
        road_mesh.road_mark_mesh.mesh.get_bevy_mesh_normals(),
    );
    roadmark_mesh.insert_attribute(
        Mesh::ATTRIBUTE_UV_0,
        road_mesh.road_mark_mesh.mesh.get_bevy_mesh_uv(),
    );
    roadmark_mesh.set_indices(Some(mesh::Indices::U32(
        road_mesh.road_mark_mesh.mesh.indicies.clone(),
    )));

    commands.spawn((
        PbrBundle {
            mesh: meshes.add(roadmark_mesh),
            material: materials.add(Color::rgba(1.0, 1.0, 1.0, 1.0).into()),
            ..default()
        },
        //Wireframe,
    ));

    if to_debug {
        // debug mesh vertex
        let ball = meshes.add(
            shape::Icosphere {
                radius: 0.1,
                subdivisions: 5,
            }
            .try_into()
            .unwrap(),
        );
        for pos in road_mesh.lane_mesh.mesh.get_bevy_mesh_position(0.0).iter() {
            commands.spawn(PbrBundle {
                mesh: ball.clone(),
                transform: Transform::from_xyz(pos.x, pos.y, pos.z),
                ..default()
            });
        }
    }

    commands.spawn(DirectionalLightBundle::default());

    let spawn_transform = odr.0.get_road_transform(&"52".to_string(), 1, 0.0).unwrap();
    let spawn_direction = &spawn_transform.direction();
    // Bevy coordinate system is Y up
    let length = 2.5;
    let width = 1.0;
    let height = 1.0;
    let box_mesh = meshes.add(Box::new(width, height, length).try_into().unwrap());

    let mut transform = Transform::from_xyz(
        spawn_transform.position().0 as f32,
        spawn_transform.position().1 as f32,
        spawn_transform.position().2 as f32 + height / 2.0,
    );

    transform.look_to(
        Vec3::new(
            spawn_direction.0 as f32,
            spawn_direction.1 as f32,
            spawn_direction.2 as f32,
        ),
        Vec3::Y,
    );

    commands.spawn((
        PbrBundle {
            mesh: box_mesh.clone(),
            transform: transform,
            material: materials.add(Color::rgba(1.0, 0.0, 0.0, 1.0).into()),
            ..default()
        },
        Actor {
            road_id: "52".to_string(),
            lane_id: 1,
            s: 20.0,
            height: height,
        },
    ));
}

fn update_actor(
    time: Res<Time>,
    odr: Res<BevyOpenDriveWrapper>,
    mut query: Query<(&mut Transform, &mut Actor), With<Actor>>,
) {
    let speed = 1.0;

    for (mut transform, mut actor) in query.iter_mut() {
        let s = actor.s + speed * time.delta_seconds_f64();
        let spawn_transform = odr
            .0
            .get_road_transform(&actor.road_id, actor.lane_id, s)
            .unwrap();
        let spawn_direction = &spawn_transform.direction();

        *transform = Transform::from_xyz(
            spawn_transform.position().0 as f32,
            spawn_transform.position().1 as f32,
            spawn_transform.position().2 as f32 + actor.height / 2.0,
        );

        transform.look_to(
            Vec3::new(
                spawn_direction.0 as f32,
                spawn_direction.1 as f32,
                spawn_direction.2 as f32,
            ),
            Vec3::Y,
        );

        actor.s = s;
    }
}
