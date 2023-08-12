use crate::open_drive::geometry::cubic_spline::{CubicSpline, Poly3};
use bevy::prelude::ParallaxMappingMethod;
use roxmltree;
use std::collections::{BTreeMap, BTreeSet, HashMap};

use super::road_mark::RoadMarkGroup;

#[derive(Debug)]
pub struct LaneKey {
    pub road_id: String,
    pub lanesection_s0: f64,
    pub lane_id: i32,
}

#[derive(Debug)]
pub struct Lane {
    pub id: i32,
    pub key: LaneKey,
    pub lane_type: String,
    pub lane_width: CubicSpline,
    pub outer_border: CubicSpline,
    pub inner_border: CubicSpline,
    pub predecessor: Option<u32>,
    pub successor: Option<u32>,
    pub roadmark_groups: BTreeSet<RoadMarkGroup>,
}

impl Lane {
    pub fn parse_lanes(
        lanesection_nodes: &mut roxmltree::Node,
        parent_road_id: &String,
        lanesection_s: f64,
    ) -> BTreeMap<i32, Lane> {
        let mut lanes = BTreeMap::new();
        for lane_node in lanesection_nodes
            .descendants()
            .filter(|&node| node.has_tag_name("lane"))
        {
            let lane_id = lane_node.attribute("id").unwrap_or("0").parse().unwrap();
            let lane_type = lane_node.attribute("type").unwrap_or("");
            let mut lane = Lane {
                id: lane_id,
                key: LaneKey {
                    road_id: parent_road_id.to_owned(),
                    lanesection_s0: lanesection_s,
                    lane_id: lane_id,
                },
                lane_type: lane_type.to_string(),
                lane_width: CubicSpline::new(),
                outer_border: CubicSpline::new(),
                inner_border: CubicSpline::new(),
                predecessor: None,
                successor: None,
                roadmark_groups: BTreeSet::new(),
            };

            if let Some(link_node) = lane_node.children().find(|&node| node.has_tag_name("link")) {
                if let Some(predecessor_node) = link_node
                    .children()
                    .find(|&node| node.has_tag_name("predecessor"))
                {
                    lane.predecessor = predecessor_node.attribute("id").unwrap_or("0").parse().ok();
                }
                if let Some(successor_node) = link_node
                    .children()
                    .find(|&node| node.has_tag_name("successor"))
                {
                    lane.predecessor = successor_node.attribute("id").unwrap_or("0").parse().ok();
                }
            }

            for lane_width_node in lane_node
                .children()
                .filter(|&node| node.has_tag_name("width"))
            {
                let s_offset = lane_width_node
                    .attribute("sOffset")
                    .unwrap_or("0.0")
                    .parse::<f64>()
                    .unwrap();
                let a = lane_width_node
                    .attribute("a")
                    .unwrap_or("0.0")
                    .parse()
                    .unwrap();
                let b = lane_width_node
                    .attribute("b")
                    .unwrap_or("0.0")
                    .parse()
                    .unwrap();
                let c = lane_width_node
                    .attribute("c")
                    .unwrap_or("0.0")
                    .parse()
                    .unwrap();
                let d = lane_width_node
                    .attribute("d")
                    .unwrap_or("0.0")
                    .parse()
                    .unwrap();
                lane.lane_width
                    .append_poly3(lanesection_s + s_offset, Poly3::new(a, b, c, d))
            }

            // TODO parse lane height
            // TODO parse road marks
            for roadmark_node in lane_node
                .children()
                .filter(|&node| node.has_tag_name("roadMark"))
            {
                if parent_road_id == "0" && lane_id == 2 {
                    println!("{:?}", roadmark_node.attributes());
                }
                lane.roadmark_groups
                    .insert(RoadMarkGroup::parse_road_mark_group(
                        &roadmark_node,
                        lane_id,
                        lanesection_s,
                        parent_road_id,
                    ));
            }
            if parent_road_id == "0" && lane_id == 2 {
                println!("{:?}", lane.roadmark_groups);
            }
            lanes.insert(lane_id, lane);
        }
        lanes
    }
}
