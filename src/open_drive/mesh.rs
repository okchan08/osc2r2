use crate::open_drive::math::{Vec2, Vec3};
use std::vec::Vec;

#[derive(Debug)]
pub struct Mesh3D {
    vertices: Vec<Vec3>,
    indicies: Vec<u32>,
    normals: Vec<Vec3>,
    st_coordinates: Vec<Vec2>,
}

#[derive(Debug)]
pub struct RoadNetworkMesh {}

impl RoadNetworkMesh {
    pub fn get_mesh(&self) -> Mesh3D {
        Mesh3D {
            vertices: vec![],
            indicies: vec![],
            normals: vec![],
            st_coordinates: vec![],
        }
    }
}
