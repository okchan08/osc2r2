use bevy::{
    input::mouse::{MouseMotion, MouseWheel},
    pbr::wireframe::WireframePlugin,
    prelude::*,
    render::render_resource::{PrimitiveTopology, WgpuFeatures},
    render::settings::WgpuSettings,
    render::{mesh, RenderPlugin},
};

use clap::Parser;
use osc2r2::bevy::bridge::BevyOpenDriveWrapper;

#[derive(Parser)]
struct Args {
    /// Path to the input OpenDRIVE file.
    #[arg(short = 'i', long = "input")]
    odr_filepath: String,
}

fn main() {
    let args = Args::parse();
    let odr = BevyOpenDriveWrapper {
        open_drive: osc2r2::open_drive::OpenDrive::parse_open_drive(args.odr_filepath),
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
        .add_systems(Startup, setup)
        .add_systems(Update, pan_orbit_camera)
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
            let yaw = Quat::from_rotation_y(delta_x);
            let pitch = Quat::from_rotation_x(-delta_y);
            transform.rotation *= yaw;
            transform.rotation *= pitch;
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

fn setup(
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
