use crate::open_drive::{
    mesh::{LaneMesh, RoadMarkMesh, RoadNetworkMesh},
    road::Road,
};

use std::collections::HashMap;

use std::fs;
use std::path::Path;

use crate::open_drive::math::Vec3;
use crate::open_drive::transform::Transform;

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

    pub fn get_road_transform(&self, road_id: &String, lane_id: i32, s: f64) -> Option<Transform> {
        if let Some(road) = self.id_to_road.get(road_id) {
            if let Some(lanesection) = road.get_lanesection(s) {
                if let Some(lane) = lanesection.id_to_lane.get(&lane_id) {
                    let t_inner = lane.inner_border.get(s, 0.0, true);
                    let t_outer = lane.outer_border.get(s, 0.0, true);
                    let t = (t_inner + t_outer) / 2.0;
                    let position = road.get_xyz(s, t, 0.0, None);
                    let rotation = road.get_direction(s);
                    return Some(Transform::new(position, rotation));
                }
            }
        }
        None
    }

    pub fn get_road_network_mesh(&self, eps: f64) -> RoadNetworkMesh {
        let mut lane_mesh = LaneMesh::new();
        let mut roadmark_mesh = RoadMarkMesh::new();
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

                    let roadmark_idx_offset = roadmark_mesh.mesh.vertices.len();
                    roadmark_mesh
                        .lane_start_indices
                        .insert(roadmark_idx_offset, lane.id);
                    let roadmarks = lane
                        .get_roadmarks(lanesection.s0, road.get_lanesection_end(lanesection.s0));

                    for roadmark in &roadmarks {
                        let roadmarks_idx_offset = roadmark_mesh.mesh.vertices.len();
                        roadmark_mesh
                            .roadmark_type_start_indicies
                            .insert(roadmarks_idx_offset, roadmark.mark_type.to_owned());
                        roadmark_mesh
                            .mesh
                            .add_mesh(&road.get_roadmark_mesh(lane, roadmark, eps));
                    }
                }
            }
        }

        RoadNetworkMesh {
            lane_mesh: lane_mesh,
            road_mark_mesh: roadmark_mesh,
        }
    }
}
