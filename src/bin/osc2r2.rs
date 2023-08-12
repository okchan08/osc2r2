use osc2r2;

use bevy::{
    pbr::wireframe::{Wireframe, WireframeConfig, WireframePlugin},
    prelude::*,
    render::render_resource::{PrimitiveTopology, WgpuFeatures},
    render::settings::WgpuSettings,
    render::{mesh, RenderPlugin},
};

fn main() {
    App::new()
        .insert_resource(ClearColor(Color::DARK_GRAY))
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
        .add_systems(Update, update_camera_transform)
        .run();
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut wireframe_config: ResMut<WireframeConfig>,
) {
    let odr = osc2r2::open_drive::open_drive::OpenDrive::parse_open_drive("./curve_r100.xodr");

    let road_mesh = odr.get_road_network_mesh(0.1);
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

    commands.spawn(Camera3dBundle {
        camera: Camera {
            hdr: true, // 1. HDR is required for bloom
            ..default()
        },
        transform: Transform::from_xyz(500.0, 25.0, 50.0).looking_at(Vec3::ZERO, Vec3::Z),
        ..default()
    });

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
}

fn update_camera_transform(
    mut camera_query: Query<&mut Transform, With<Camera>>,
    keycode: Res<Input<KeyCode>>,
) {
    let mut camera_transform = camera_query.single_mut();

    let mut move_forward = 0.0f32;
    let mut turn_left = 0.0f32;
    let mut turn_up = 0.0f32;
    const MOVE_UNIT: f32 = 5.0;
    const TURN_UNIT: f32 = 0.01;
    if keycode.pressed(KeyCode::Up) {
        move_forward += MOVE_UNIT;
    }
    if keycode.pressed(KeyCode::Down) {
        move_forward -= MOVE_UNIT;
    }
    if keycode.pressed(KeyCode::D) {
        turn_left -= TURN_UNIT;
    }
    if keycode.pressed(KeyCode::A) {
        turn_left += TURN_UNIT;
    }
    if keycode.pressed(KeyCode::W) {
        turn_up += TURN_UNIT;
    }
    if keycode.pressed(KeyCode::S) {
        turn_up -= TURN_UNIT;
    }

    if move_forward != 0.0 {
        let translation = camera_transform.forward() * move_forward;
        camera_transform.translation += translation;
    }

    if turn_left != 0.0 {
        let rotation = Quat::from_rotation_z(turn_left);
        camera_transform.rotate(rotation);
    }

    if turn_up != 0.0 {
        let rotation = Quat::from_rotation_y(turn_up);
        camera_transform.rotate(rotation);
    }
}
