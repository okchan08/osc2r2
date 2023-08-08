use super::mesh::RoadNetworkMesh;

#[derive(Debug)]
pub struct OpenDrive {}

impl OpenDrive {
    pub fn get_road_network_mesh(&self) -> RoadNetworkMesh {
        RoadNetworkMesh {}
    }
}
