use ordered_float::OrderedFloat;
use osc2r2::{self, open_drive::lane::LaneKey};

use osc2r2::bevy::bridge::BevyOpenDriveWrapper;

use bevy::{
    input::mouse::{MouseMotion, MouseWheel},
    pbr::wireframe::WireframePlugin,
    prelude::{shape::Box, *},
    render::render_resource::{PrimitiveTopology, WgpuFeatures},
    render::settings::WgpuSettings,
    render::{mesh, RenderPlugin},
};

fn main() {
    let odr = BevyOpenDriveWrapper {
        open_drive: osc2r2::open_drive::OpenDrive::parse_open_drive("./Town02.xodr"),
    };

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
        .add_systems(Startup, (setup_world, setup_actors.after(setup_world)))
        .add_systems(Update, (PanOrbitCamera::follow_orbit_camera, update_actor))
        .run();
}

#[derive(Component)]
struct PanOrbitCamera {
    pub focus: Vec3,
    pub radius: f32,
}

impl Default for PanOrbitCamera {
    fn default() -> Self {
        Self {
            focus: Vec3::ZERO,
            radius: 5.0,
        }
    }
}

impl PanOrbitCamera {
    #[allow(clippy::type_complexity)]
    fn follow_orbit_camera(
        windows: Query<&Window>,
        actors: Query<&Transform, With<Actor>>,
        mut query: Query<(&mut PanOrbitCamera, &mut Transform), (With<Camera>, Without<Actor>)>,
        input_mouse: Res<Input<MouseButton>>,
        mut ev_scroll: EventReader<MouseWheel>,
        mut ev_motion: EventReader<MouseMotion>,
    ) {
        let Ok(actor_transform) = actors.get_single() else {return};
        let Ok((mut camera, mut camera_transform)) = query.get_single_mut() else {return};

        let delta = actor_transform.translation - camera.focus;
        let mut scroll = 0.0;
        let mut rotation_move = Vec2::ZERO;
        let mut any = false;
        let rotation_button = MouseButton::Left;
        if input_mouse.pressed(rotation_button) {
            for ev in ev_motion.iter() {
                rotation_move += ev.delta / 2.0;
            }
        }
        for ev in ev_scroll.iter() {
            scroll += ev.y / 10.0;
        }
        if rotation_move.length_squared() > 0.0 {
            any = true;
            let window = get_primary_window_size(windows.single());
            let delta_x = rotation_move.x / window.x * std::f32::consts::PI * 2.0;
            let delta_y = rotation_move.y / window.y * std::f32::consts::PI;
            let yaw = Quat::from_rotation_y(delta_x);
            let pitch = Quat::from_rotation_x(-delta_y);
            camera_transform.rotation *= yaw;
            camera_transform.rotation *= pitch;
        } else if scroll.abs() > 0.0 {
            any = true;
            camera.radius -= scroll * camera.radius * 0.2;
            // dont allow zoom to reach zero or you get stuck
            camera.radius = f32::max(camera.radius, 0.05);
        }

        if delta != Vec3::ZERO {
            any = true;
            camera.focus = actor_transform.translation;
            camera_transform.translation += delta;
        }

        if any {
            let rot_matrix = Mat3::from_quat(camera_transform.rotation);
            camera_transform.translation =
                camera.focus + rot_matrix.mul_vec3(Vec3::new(0.0, 0.0, camera.radius));
        }
    }
}

fn get_primary_window_size(window: &Window) -> Vec2 {
    Vec2::new(window.width(), window.height())
}

#[derive(Component)]
struct Actor {
    pub lane_key: LaneKey,
    pub s: f64,
    pub height: f32,
}

fn setup_world(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    odr: Res<BevyOpenDriveWrapper>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    let road_mesh = odr.open_drive.get_road_network_mesh(0.1);
    let translation = Vec3::new(10.0, 3.0, 40.0);
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
            radius,
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

    commands.spawn((PbrBundle {
        mesh: meshes.add(mesh),
        material: materials.add(Color::rgba(0.2, 0.2, 0.2, 1.0).into()),
        ..default()
    },));

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
        road_mesh.road_mark_mesh.mesh.indicies,
    )));

    commands.spawn((PbrBundle {
        mesh: meshes.add(roadmark_mesh),
        material: materials.add(Color::rgba(1.0, 1.0, 1.0, 1.0).into()),
        ..default()
    },));

    commands.spawn(DirectionalLightBundle::default());
}

fn setup_actors(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    odr: Res<BevyOpenDriveWrapper>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    let road_id = "1";
    let lane_id = 1;
    let s = 10.0;
    let spawn_transform = odr
        .open_drive
        .get_road_transform(&road_id.to_string(), &lane_id, &s)
        .unwrap();
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
            mesh: box_mesh,
            transform,
            material: materials.add(Color::rgba(1.0, 0.0, 0.0, 1.0).into()),
            ..default()
        },
        Actor {
            lane_key: LaneKey {
                road_id: road_id.to_string(),
                lanesection_s0: OrderedFloat(s),
                lane_id,
            },
            s,
            height,
        },
    ));
}

fn update_actor(
    time: Res<Time>,
    odr: Res<BevyOpenDriveWrapper>,
    mut query: Query<(&mut Transform, &mut Actor), With<Actor>>,
) {
    let speed = 10.0;

    for (mut transform, mut actor) in query.iter_mut() {
        let ds = speed * time.delta_seconds_f64();
        if let Some((lane_key, next_s)) =
            odr.open_drive
                .evaluate_road_ds(&actor.lane_key.clone(), actor.s, ds)
        {
            let Some(spawn_transform) =
                odr.open_drive
                    .get_road_transform(&lane_key.road_id, &lane_key.lane_id, &next_s) else {return;};
            // TODO fix lane id
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

            actor.s = next_s;
            actor.lane_key = lane_key.clone();
        }
    }
}
