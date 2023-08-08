use crate::open_drive::lane::Lane;
use std::collections::BTreeMap;

use roxmltree;

use super::lane;

#[derive(Debug)]
pub struct LaneSection {
    pub road_id: String,
    pub s0: f64,
    id_to_lane: BTreeMap<i32, Lane>,
}

impl LaneSection {
    pub fn parse_lane_section(
        lanesection_node: &mut roxmltree::Node,
        parent_road_id: &String,
    ) -> LaneSection {
        let s0 = lanesection_node
            .attribute("s")
            .unwrap_or("0.0")
            .parse()
            .unwrap();
        let mut lanesection = LaneSection {
            road_id: parent_road_id.to_owned(),
            s0: s0,
            id_to_lane: Lane::parse_lanes(lanesection_node, &parent_road_id, s0),
        };
        Self::derive_lane_borders(&mut lanesection);
        lanesection
    }

    fn derive_lane_borders(lanesection: &mut LaneSection) {
        let lane_id_0 = 0 as i32;
        if !lanesection.id_to_lane.contains_key(&lane_id_0) {
            panic!("lane section does not have lane #0");
        }

        // copy key array in order to avoid immutable/mutable borrowing conflicts
        let positive_keys = lanesection
            .id_to_lane
            .keys()
            .into_iter()
            .map(|key| key.to_owned())
            .filter(|&key| key >= 0)
            .collect::<Vec<i32>>();

        let negative_keys = lanesection
            .id_to_lane
            .keys()
            .into_iter()
            .rev()
            .map(|key| key.to_owned())
            .filter(|&key| key <= 0)
            .collect::<Vec<i32>>();

        for (i, lane_id) in positive_keys.iter().enumerate() {
            if *lane_id == lane_id_0 {
                assert!(i == 0);
                let mut lane = lanesection.id_to_lane.get_mut(&lane_id).unwrap();
                lane.outer_border = lane.lane_width.clone();
            } else {
                let prev_lane_id = positive_keys[i - 1];
                lanesection
                    .id_to_lane
                    .get_mut(&lane_id)
                    .unwrap()
                    .inner_border = lanesection
                    .id_to_lane
                    .get_mut(&prev_lane_id)
                    .unwrap()
                    .outer_border
                    .clone();
                lanesection
                    .id_to_lane
                    .get_mut(&lane_id)
                    .unwrap()
                    .outer_border = lanesection
                    .id_to_lane
                    .get_mut(&prev_lane_id)
                    .unwrap()
                    .outer_border
                    .clone()
                    .add(&lanesection.id_to_lane.get(&lane_id).unwrap().lane_width);
            }
        }

        for (i, lane_id) in negative_keys.iter().enumerate() {
            if *lane_id == lane_id_0 {
                assert!(i == 0);
                let mut lane = lanesection.id_to_lane.get_mut(&lane_id).unwrap();
                lane.outer_border = lane.lane_width.clone();
            } else {
                let prev_lane_id = negative_keys[i - 1];
                lanesection
                    .id_to_lane
                    .get_mut(&lane_id)
                    .unwrap()
                    .inner_border = lanesection
                    .id_to_lane
                    .get_mut(&prev_lane_id)
                    .unwrap()
                    .outer_border
                    .clone();
                lanesection
                    .id_to_lane
                    .get_mut(&lane_id)
                    .unwrap()
                    .outer_border = lanesection
                    .id_to_lane
                    .get_mut(&prev_lane_id)
                    .unwrap()
                    .outer_border
                    .clone()
                    .add(&lanesection.id_to_lane.get(&lane_id).unwrap().lane_width);
            }
        }
    }
}
