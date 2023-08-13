use crate::open_drive::lane::Lane;
use std::collections::BTreeMap;

use roxmltree;

use super::road::Road;

#[derive(Debug)]
pub struct LaneSection {
    pub road_id: String,
    pub s0: f64,
    pub id_to_lane: BTreeMap<i32, Lane>,
}

impl LaneSection {
    pub fn parse_lane_section(
        lanesection_node: &mut roxmltree::Node,
        parent_road: &Road,
    ) -> LaneSection {
        let s0 = lanesection_node
            .attribute("s")
            .unwrap_or("0.0")
            .parse()
            .unwrap();
        let mut lanesection = LaneSection {
            road_id: parent_road.id.to_owned(),
            s0: s0,
            id_to_lane: Lane::parse_lanes(lanesection_node, &parent_road.id, s0),
        };
        Self::derive_lane_borders(&mut lanesection, parent_road);
        lanesection
    }

    fn derive_lane_borders(lanesection: &mut LaneSection, parent_road: &Road) {
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
                let prev_lane_outer_brdr = lanesection
                    .id_to_lane
                    .get(&prev_lane_id)
                    .unwrap()
                    .outer_border
                    .clone();
                lanesection
                    .id_to_lane
                    .get_mut(&lane_id)
                    .unwrap()
                    .inner_border = prev_lane_outer_brdr.clone();
                lanesection
                    .id_to_lane
                    .get_mut(&lane_id)
                    .unwrap()
                    .outer_border = prev_lane_outer_brdr
                    .add(&lanesection.id_to_lane.get(&lane_id).unwrap().lane_width);
            }
        }

        for (i, lane_id) in negative_keys.iter().enumerate() {
            if *lane_id == lane_id_0 {
                assert!(i == 0);
                let mut lane = lanesection.id_to_lane.get_mut(&lane_id).unwrap();
                lane.outer_border = lane.lane_width.clone().negate();
            } else {
                let prev_lane_id = negative_keys[i - 1];
                let prev_lane_outer_brdr = lanesection
                    .id_to_lane
                    .get(&prev_lane_id)
                    .unwrap()
                    .outer_border
                    .clone();
                lanesection
                    .id_to_lane
                    .get_mut(&lane_id)
                    .unwrap()
                    .inner_border = prev_lane_outer_brdr.clone();
                lanesection
                    .id_to_lane
                    .get_mut(&lane_id)
                    .unwrap()
                    .outer_border = prev_lane_outer_brdr.add(
                    &lanesection
                        .id_to_lane
                        .get(&lane_id)
                        .unwrap()
                        .lane_width
                        .negate(),
                );
            }
        }

        for (_, mut lane) in lanesection.id_to_lane.iter_mut() {
            lane.inner_border = lane.inner_border.add(&parent_road.lane_offset);
            lane.outer_border = lane.outer_border.add(&parent_road.lane_offset);
        }
    }

    pub fn get_lane_id(&self, s: f64, t: f64) -> i32 {
        if self
            .id_to_lane
            .get(&0)
            .unwrap()
            .outer_border
            .get(s, 0.0, true)
            == t
        {
            return 0;
        }

        for (lane_id, lane) in self.id_to_lane.iter() {
            let outer_brdr_t = lane.outer_border.get(s, 0.0, true);
            let inner_brdr_t = lane.inner_border.get(s, 0.0, true);
            if (*lane_id < 0 && outer_brdr_t <= t && t <= inner_brdr_t)
                || (*lane_id > 0 && inner_brdr_t <= t && t <= outer_brdr_t)
            {
                return *lane_id;
            }
        }
        // TODO this lane id is wrong, need to fix the search algorithm.
        return 0;
    }
}
