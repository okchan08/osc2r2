use crate::open_drive::{
    mesh::{LaneMesh, RoadNetworkMesh},
    road::Road,
};

use std::collections::HashMap;

use std::fs;
use std::path::Path;

#[derive(Debug)]
pub struct OpenDrive {
    id_to_road: HashMap<String, Road>,
}

impl OpenDrive {
    pub fn parse_open_drive<T: AsRef<Path>>(path: T) -> OpenDrive {
        let s = fs::read(path.as_ref()).unwrap();
        let binding = String::from_utf8(s).unwrap();
        let contents = binding.as_str();
        let doc = roxmltree::Document::parse(&contents).unwrap();
        let mut node = doc.root_element();

        OpenDrive {
            id_to_road: Road::parse_roads(&mut node),
        }
    }

    pub fn get_road_network_mesh(&self, eps: f64) -> RoadNetworkMesh {
        let mut lane_mesh = LaneMesh::new();
        for (road_id, road) in self.id_to_road.iter() {
            lane_mesh
                .road_start_indicies
                .insert(lane_mesh.mesh.vertices.len(), road_id.to_owned());

            for (_, lanesection) in road.s_to_lanesection.iter() {
                lane_mesh
                    .lanesec_start_indices
                    .insert(lane_mesh.mesh.vertices.len(), lanesection.s0);

                for (lane_id, lane) in lanesection.id_to_lane.iter() {
                    let lanes_idx_offset = lane_mesh.mesh.vertices.len();
                    lane_mesh
                        .lane_start_indices
                        .insert(lanes_idx_offset, *lane_id);
                    lane_mesh.mesh.add_mesh(&road.get_lane_mesh(lane, eps));
                }
            }
        }

        RoadNetworkMesh {
            lane_mesh: lane_mesh,
            road_mark_mesh: None,
        }
    }
}
