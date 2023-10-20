mod geometry;
mod junction;
pub mod lane;
mod lane_section;
mod math;
mod mesh;
mod ref_line;
mod road;
mod road_mark;
mod road_topology;
mod transform;
mod utils;

use ordered_float::OrderedFloat;
use rand::{thread_rng, Rng};

use crate::open_drive::{
    mesh::{LaneMesh, RoadMarkMesh, RoadNetworkMesh},
    road::Road,
};

use std::{collections::HashMap, ops::Bound};

use std::fs;
use std::path::Path;

use transform::Transform;

use junction::{Junction, JunctionConnection};
use lane::LaneKey;
use road::{RoadContactPoint, RoadLinkType};
use road_topology::{RoadEdge, RoadTopology};

#[derive(Debug)]
pub struct OpenDrive {
    id_to_road: HashMap<String, Road>,
    id_to_junction: HashMap<String, Junction>,
    road_topology: RoadTopology,
}

impl OpenDrive {
    pub fn parse_open_drive<T: AsRef<Path>>(path: T) -> OpenDrive {
        let s = fs::read(path.as_ref()).unwrap();
        let binding = String::from_utf8(s).unwrap();
        let contents = binding.as_str();
        let doc = roxmltree::Document::parse(contents).unwrap();
        let node = doc.root_element();

        let mut odr = OpenDrive {
            id_to_road: Road::parse_roads(&node),
            id_to_junction: Junction::parse_junctions(&node),
            road_topology: RoadTopology::default(),
        };

        odr.construct_road_topology();
        odr.traverse_junctions();

        odr
    }

    fn construct_road_topology(&mut self) {
        for find_successor in vec![true, false].into_iter() {
            for (_, road) in self.id_to_road.iter() {
                let road_link_opt = if find_successor {
                    &road.successor
                } else {
                    &road.predecessor
                };
                if road_link_opt.is_none() {
                    continue;
                }
                let road_link = road_link_opt.as_ref().unwrap();
                if road_link.link_type != RoadLinkType::Road
                    || road_link.contact_point == RoadContactPoint::None
                {
                    continue;
                }

                if !self.id_to_road.contains_key(&road_link.id) {
                    continue;
                }

                let Some(next_road) = self.id_to_road.get(&road_link.id) else {continue;};
                let next_road_contact_lanesection =
                    if road_link.contact_point == RoadContactPoint::Start {
                        next_road.s_to_lanesection.first_key_value().unwrap().1
                    } else {
                        next_road.s_to_lanesection.last_key_value().unwrap().1
                    };

                for (s0, lane_section) in road.s_to_lanesection.iter() {
                    let next_lanesec_s0 = road
                        .s_to_lanesection
                        .range((Bound::Excluded(s0), Bound::Unbounded))
                        .next();
                    let (next_lanesec, next_lanesec_road) = if (find_successor
                        && next_lanesec_s0.is_none())
                        || (!find_successor
                            && s0 == road.s_to_lanesection.first_key_value().unwrap().0)
                    {
                        (next_road_contact_lanesection, next_road)
                    } else if find_successor {
                        (
                            road.s_to_lanesection
                                .get(next_lanesec_s0.unwrap().0)
                                .unwrap(),
                            road,
                        )
                    } else {
                        let prev_lanesec_s0 =
                            road.s_to_lanesection.range(..s0).next_back().unwrap().0;
                        (road.s_to_lanesection.get(prev_lanesec_s0).unwrap(), road)
                    };
                    for (_, lane) in lane_section.id_to_lane.iter() {
                        let next_lane_id_opt = if find_successor {
                            lane.successor
                        } else {
                            lane.predecessor
                        };
                        if next_lane_id_opt.is_none() {
                            continue;
                        }
                        let next_lane_id = next_lane_id_opt.unwrap();
                        if next_lane_id == 0 {
                            continue;
                        }

                        let next_lane_opt = next_lanesec.id_to_lane.get(&next_lane_id);
                        if next_lane_opt.is_none() {
                            continue;
                        }
                        let next_lane = next_lane_opt.unwrap();

                        let (from_lane, to_lane) = if find_successor {
                            (lane, next_lane)
                        } else {
                            (next_lane, lane)
                        };
                        let (from_lanesection, to_lanesection) = if find_successor {
                            (lane_section, next_lanesec)
                        } else {
                            (next_lanesec, lane_section)
                        };
                        let (from_road, to_road) = if find_successor {
                            (road, next_lanesec_road)
                        } else {
                            (next_lanesec_road, road)
                        };

                        let from = LaneKey {
                            road_id: from_road.id.clone(),
                            lanesection_s0: OrderedFloat(from_lanesection.s0),
                            lane_id: from_lane.id,
                        };

                        let to = LaneKey {
                            road_id: to_road.id.clone(),
                            lanesection_s0: OrderedFloat(to_lanesection.s0),
                            lane_id: to_lane.id,
                        };

                        self.road_topology.add_edge(RoadEdge { from, to });
                    }
                }
            }
        }
    }

    fn traverse_junctions(&mut self) {
        for (junction_id, junction) in self.id_to_junction.iter() {
            for (_, connection) in junction.id_to_connection.iter() {
                if let (Some(connecting_road), Some(incoming_road)) = (
                    self.id_to_road.get(&connection.get_info().connecting_road),
                    self.id_to_road.get(&connection.get_info().incoming_road),
                ) {
                    let is_succ_junc = incoming_road.successor.is_some()
                        && incoming_road.successor.as_ref().unwrap().link_type
                            == RoadLinkType::Junction
                        && &incoming_road.successor.as_ref().unwrap().id == junction_id;
                    let is_pred_junc = incoming_road.predecessor.is_some()
                        && incoming_road.predecessor.as_ref().unwrap().link_type
                            == RoadLinkType::Junction
                        && &incoming_road.predecessor.as_ref().unwrap().id == junction_id;
                    if !is_succ_junc && !is_pred_junc {
                        continue;
                    }

                    let incoming_lanesec = if is_succ_junc {
                        incoming_road.s_to_lanesection.last_key_value().unwrap().1
                    } else {
                        incoming_road.s_to_lanesection.first_key_value().unwrap().1
                    };
                    let connecting_lanesec = match connection {
                        JunctionConnection::Start(_) => {
                            connecting_road
                                .s_to_lanesection
                                .first_key_value()
                                .unwrap()
                                .1
                        }
                        _ => connecting_road.s_to_lanesection.last_key_value().unwrap().1,
                    };

                    for lane_link in connection.get_info().lane_links.iter() {
                        if lane_link.from == 0 || lane_link.to == 0 {
                            continue;
                        }
                        if let (Some(from_lane), Some(to_lane)) = (
                            incoming_lanesec.id_to_lane.get(&lane_link.from),
                            connecting_lanesec.id_to_lane.get(&lane_link.to),
                        ) {
                            let from = LaneKey {
                                road_id: incoming_road.id.clone(),
                                lanesection_s0: OrderedFloat(incoming_lanesec.s0),
                                lane_id: from_lane.id,
                            };

                            let to = LaneKey {
                                road_id: connecting_road.id.clone(),
                                lanesection_s0: OrderedFloat(connecting_lanesec.s0),
                                lane_id: to_lane.id,
                            };

                            self.road_topology.add_edge(RoadEdge { from, to });
                        }
                    }
                }
            }
        }
    }

    pub fn get_road_transform(
        &self,
        road_id: &String,
        lane_id: &i32,
        s: &f64,
    ) -> Option<Transform> {
        if let Some(road) = self.id_to_road.get(road_id) {
            if let Some(lanesection) = road.get_lanesection(*s) {
                if let Some(lane) = lanesection.id_to_lane.get(lane_id) {
                    let t_inner = lane.inner_border.get(*s, 0.0, true);
                    let t_outer = lane.outer_border.get(*s, 0.0, true);
                    let t = (t_inner + t_outer) / 2.0;
                    let position = road.get_xyz(*s, t, 0.0, None);
                    let rotation = road.get_direction(*s);
                    return Some(Transform::new(position, rotation));
                }
            }
        }
        None
    }

    pub fn evaluate_road_ds<'a>(
        &'a self,
        lane_key: &'a LaneKey,
        s: f64,
        ds: f64,
    ) -> Option<(&LaneKey, f64)> {
        if let Some(road) = self.id_to_road.get(&lane_key.road_id) {
            // assume right hand traffic
            let new_s = s + ds * if lane_key.lane_id > 0 { -1.0 } else { 1.0 };
            match road.get_lanesection(new_s) {
                None => {
                    let lane_candidates = if lane_key.lane_id < 0 {
                        let last_lanesec_s0 = road.s_to_lanesection.last_key_value().unwrap().0;
                        self.road_topology.get_lane_successor(&LaneKey {
                            road_id: lane_key.road_id.clone(),
                            lanesection_s0: *last_lanesec_s0,
                            lane_id: lane_key.lane_id,
                        })?
                    } else {
                        let first_lanesec_s0 = road.s_to_lanesection.first_key_value().unwrap().0;
                        self.road_topology.get_lane_predecessor(&LaneKey {
                            road_id: lane_key.road_id.clone(),
                            lanesection_s0: *first_lanesec_s0,
                            lane_id: lane_key.lane_id,
                        })?
                    };

                    // choose first connecting lane.
                    let next_lane_key = {
                        let len = lane_candidates.len();
                        let mut rand_gen = thread_rng();
                        let num = rand_gen.gen_range(0..len);

                        lane_candidates.iter().nth(num)?
                    };

                    let next_s = if lane_key.lane_id < 0 {
                        let delta_s = new_s - road.length;
                        assert!(delta_s > 0.0);
                        delta_s
                    } else {
                        let delta_s = new_s.abs();
                        let next_road = self.id_to_road.get(&next_lane_key.road_id)?;
                        next_road.length - delta_s
                    };

                    return Some((next_lane_key, next_s));
                }
                Some(_) => {
                    return Some((lane_key, new_s));
                }
            }
        }
        None
    }

    pub fn get_road_network_mesh(&self, eps: f64) -> RoadNetworkMesh {
        let mut lane_mesh = LaneMesh::default();
        let mut roadmark_mesh = RoadMarkMesh::default();
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
            lane_mesh,
            road_mark_mesh: roadmark_mesh,
        }
    }
}
