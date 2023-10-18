use bevy::utils::HashMap;

use super::lane::LaneKey;

use std::collections::{BTreeSet, HashSet};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct RoadEdge {
    pub from: LaneKey,
    pub to: LaneKey,
}

#[derive(Debug, Default)]
pub struct RoadTopology {
    edges: HashSet<RoadEdge>,
    lane_key_to_successors: HashMap<LaneKey, BTreeSet<LaneKey>>,
    lane_key_to_predessors: HashMap<LaneKey, BTreeSet<LaneKey>>,
}

impl RoadTopology {
    pub fn add_edge(&mut self, edge: RoadEdge) {
        match self.lane_key_to_successors.get_mut(&edge.from) {
            None => {
                let new_set = [edge.to.clone()].into_iter().collect();
                self.lane_key_to_successors
                    .insert(edge.from.clone(), new_set);
            }
            Some(hashset) => {
                hashset.insert(edge.to.clone());
            }
        }
        match self.lane_key_to_predessors.get_mut(&edge.to) {
            None => {
                let new_set = [edge.from.clone()].into_iter().collect();
                self.lane_key_to_predessors.insert(edge.to.clone(), new_set);
            }
            Some(hashset) => {
                hashset.insert(edge.from.clone());
            }
        }
        self.edges.insert(edge);
    }

    pub fn get_lane_successor(&self, lane_key: &LaneKey) -> Option<&BTreeSet<LaneKey>> {
        self.lane_key_to_successors.get(lane_key)
    }

    pub fn get_lane_predecessor(&self, lane_key: &LaneKey) -> Option<&BTreeSet<LaneKey>> {
        self.lane_key_to_predessors.get(lane_key)
    }
}
