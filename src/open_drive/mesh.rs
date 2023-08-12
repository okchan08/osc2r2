use crate::open_drive::math::{Vec2, Vec3};
use std::collections::HashMap;
use std::vec::Vec;

#[derive(Debug, Clone)]
pub struct Mesh3D {
    pub vertices: Vec<Vec3>,
    pub indicies: Vec<u32>,
    pub normals: Vec<Vec3>,
    pub st_coordinates: Vec<Vec2>,
}

impl Mesh3D {
    pub fn new() -> Self {
        Self {
            vertices: vec![],
            indicies: vec![],
            normals: vec![],
            st_coordinates: vec![],
        }
    }

    pub fn add_mesh(&mut self, other: &Mesh3D) {
        let idx_offset = self.vertices.len() as u32;

        self.vertices.extend(&other.vertices);
        self.normals.extend(&other.normals);
        self.st_coordinates.extend(&other.st_coordinates);

        for idx in other.indicies.iter() {
            self.indicies.push(*idx + idx_offset);
        }
    }

    pub fn get_bevy_mesh_position(&self, z_bias: f32) -> Vec<bevy::prelude::Vec3> {
        // TODO : in order to avoid z-fighting in a mesh, add a z bias only in vec.z.
        //        however this does not work fine in a 3D road.
        let mut retval = vec![];
        for v in &self.vertices {
            retval.push(bevy::prelude::Vec3::new(
                v.0 as f32,
                v.1 as f32,
                v.2 as f32 + z_bias,
            ));
        }
        retval
    }

    pub fn get_bevy_mesh_normals(&self) -> Vec<bevy::prelude::Vec3> {
        let mut retval = vec![];
        for v in &self.normals {
            retval.push(bevy::prelude::Vec3::new(v.0 as f32, v.1 as f32, v.2 as f32));
        }
        retval
    }

    pub fn get_bevy_mesh_uv(&self) -> Vec<bevy::prelude::Vec2> {
        vec![bevy::prelude::Vec2::new(0.0, 0.0); self.vertices.len()]
    }
}

#[derive(Debug)]
pub struct RoadMesh {
    pub mesh: Mesh3D,
    pub road_start_indicies: HashMap<usize, String>,
}

impl RoadMesh {
    pub fn new() -> Self {
        Self {
            mesh: Mesh3D::new(),
            road_start_indicies: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct LaneMesh {
    pub mesh: Mesh3D,
    pub road_start_indicies: HashMap<usize, String>,
    pub lanesec_start_indices: HashMap<usize, f64>,
    pub lane_start_indices: HashMap<usize, i32>,
}

impl LaneMesh {
    pub fn new() -> Self {
        LaneMesh {
            mesh: Mesh3D::new(),
            road_start_indicies: HashMap::new(),
            lanesec_start_indices: HashMap::new(),
            lane_start_indices: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct RoadMarkMesh {
    pub mesh: Mesh3D,
    pub road_start_indicies: HashMap<usize, String>,
    pub lanesec_start_indices: HashMap<usize, f64>,
    pub lane_start_indices: HashMap<usize, i32>,
    pub roadmark_type_start_indicies: HashMap<usize, String>,
}

impl RoadMarkMesh {
    pub fn new() -> Self {
        Self {
            mesh: Mesh3D::new(),
            road_start_indicies: HashMap::new(),
            lanesec_start_indices: HashMap::new(),
            lane_start_indices: HashMap::new(),
            roadmark_type_start_indicies: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct RoadNetworkMesh {
    pub lane_mesh: LaneMesh,
    pub road_mark_mesh: RoadMarkMesh,
}

impl RoadNetworkMesh {
    pub fn get_mesh(&self) -> Mesh3D {
        let mut out_mesh = Mesh3D::new();

        out_mesh.add_mesh(&self.lane_mesh.mesh);
        out_mesh
    }
}
