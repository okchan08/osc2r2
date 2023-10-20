use crate::open_drive::lane::Lane;
use std::collections::BTreeMap;

use ordered_float::OrderedFloat;
use roxmltree;

use super::road::Road;

#[derive(Debug)]
pub struct LaneSection {
    pub road_id: String,
    pub s0: f64,
    pub s_end: f64,
    pub id_to_lane: BTreeMap<i32, Lane>,
}

impl LaneSection {
    pub fn parse_lane_section(
        lanesection_node: &roxmltree::Node,
        parent_road: &Road,
    ) -> LaneSection {
        let s0 = lanesection_node
            .attribute("s")
            .unwrap_or("0.0")
            .parse()
            .unwrap();
        let mut lanesection = LaneSection {
            road_id: parent_road.id.to_owned(),
            s0,
            s_end: f64::NAN,
            id_to_lane: Lane::parse_lanes(lanesection_node, &parent_road.id, s0),
        };
        Self::derive_lane_borders(&mut lanesection, parent_road);
        lanesection
    }

    fn derive_lane_borders(lanesection: &mut LaneSection, parent_road: &Road) {
        let lane_id_0 = 0_i32;
        if !lanesection.id_to_lane.contains_key(&lane_id_0) {
            panic!("lane section does not have lane #0");
        }

        // copy key array in order to avoid immutable/mutable borrowing conflicts
        let positive_keys = lanesection
            .id_to_lane
            .keys()
            .map(|key| key.to_owned())
            .filter(|&key| key >= 0)
            .collect::<Vec<i32>>();

        let negative_keys = lanesection
            .id_to_lane
            .keys()
            .rev()
            .map(|key| key.to_owned())
            .filter(|&key| key <= 0)
            .collect::<Vec<i32>>();

        for (i, lane_id) in positive_keys.iter().enumerate() {
            if *lane_id == lane_id_0 {
                assert!(i == 0);
                let mut lane = lanesection.id_to_lane.get_mut(lane_id).unwrap();
                lane.outer_border = lane.lane_width.clone();
            } else {
                let prev_lane_id = &positive_keys[i - 1];
                let prev_lane_outer_brdr = lanesection
                    .id_to_lane
                    .get(prev_lane_id)
                    .unwrap()
                    .outer_border
                    .clone();
                let current_lane = lanesection.id_to_lane.get_mut(lane_id).unwrap();
                current_lane.inner_border = prev_lane_outer_brdr.clone();
                current_lane.outer_border = prev_lane_outer_brdr.add(&current_lane.lane_width);
            }
        }

        for (i, lane_id) in negative_keys.iter().enumerate() {
            if *lane_id == lane_id_0 {
                assert!(i == 0);
                let mut lane = lanesection.id_to_lane.get_mut(lane_id).unwrap();
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
                    .get_mut(lane_id)
                    .unwrap()
                    .inner_border = prev_lane_outer_brdr.clone();
                lanesection
                    .id_to_lane
                    .get_mut(lane_id)
                    .unwrap()
                    .outer_border = prev_lane_outer_brdr.add(
                    &lanesection
                        .id_to_lane
                        .get(lane_id)
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

    #[allow(unused_assignments)]
    pub fn get_lane_id(&self, s: f64, t: f64) -> Option<i32> {
        if self
            .id_to_lane
            .get(&0)
            .unwrap()
            .outer_border
            .get(s, 0.0, true)
            == t
        {
            return Some(0);
        }

        // TODO waiting for Rust to implement BTreeMap::Cursor https://doc.rust-lang.org/std/collections/btree_map/struct.Cursor.html
        let outer_t_to_lane_id = self
            .id_to_lane
            .iter()
            .map(|(lane_id, lane)| {
                let outer_t = lane.outer_border.get(s, 0.0, true);
                (OrderedFloat(outer_t), *lane_id)
            })
            .collect::<BTreeMap<OrderedFloat<f64>, i32>>();

        let mut border_iter = outer_t_to_lane_id.range(OrderedFloat(t)..);
        let mut current_candidate = None;
        match border_iter.next() {
            None => {
                // Reaches the end of the map.
                border_iter = outer_t_to_lane_id
                    .range(OrderedFloat(-f64::INFINITY)..(OrderedFloat(f64::INFINITY)));
                current_candidate = border_iter.next_back();
            }
            Some(val) => {
                current_candidate = Some(val);
            }
        }

        match current_candidate {
            None => None,
            Some(val) => {
                if *val.1 <= 0 && val.0 .0 != t {
                    match border_iter.next_back() {
                        None => Some(*val.1),
                        Some(final_val) => Some(*final_val.1),
                    }
                } else {
                    Some(*val.1)
                }
            }
        }
    }
}
