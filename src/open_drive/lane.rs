use crate::open_drive::geometry::cubic_spline::{CubicSpline, Poly3};
use ordered_float::OrderedFloat;
use roxmltree;
use std::collections::{BTreeMap, BTreeSet};

use super::road_mark::{RoadMark, RoadMarkGroup};

#[derive(Debug, PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
pub struct LaneKey {
    pub road_id: String,
    pub lanesection_s0: OrderedFloat<f64>,
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
    pub predecessor: Option<i32>,
    pub successor: Option<i32>,
    pub roadmark_groups: BTreeSet<RoadMarkGroup>,
}

impl Lane {
    pub fn parse_lanes(
        lanesection_nodes: &roxmltree::Node,
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
                    lanesection_s0: OrderedFloat(lanesection_s),
                    lane_id,
                },
                lane_type: lane_type.to_string(),
                lane_width: CubicSpline::default(),
                outer_border: CubicSpline::default(),
                inner_border: CubicSpline::default(),
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
                    lane.successor = successor_node.attribute("id").unwrap_or("0").parse().ok();
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
                lane.lane_width.append_poly3(
                    lanesection_s + s_offset,
                    Poly3::new(lanesection_s + s_offset, a, b, c, d),
                )
            }

            // TODO parse lane height
            // TODO parse road marks
            for roadmark_node in lane_node
                .children()
                .filter(|&node| node.has_tag_name("roadMark"))
            {
                lane.roadmark_groups
                    .insert(RoadMarkGroup::parse_road_mark_group(
                        &roadmark_node,
                        lane_id,
                        lanesection_s,
                        parent_road_id,
                    ));
            }
            lanes.insert(lane_id, lane);
        }
        lanes
    }

    pub fn get_roadmarks(&self, s_start: f64, s_end: f64) -> Vec<RoadMark> {
        if s_start == s_end || self.roadmark_groups.is_empty() {
            return vec![];
        }

        let mut roadmarks = vec![];
        let mut roadmark_group_iter = self.roadmark_groups.iter().peekable();

        while let Some(roadmark_group) = roadmark_group_iter.next() {
            let roadmark_group_s0 = roadmark_group.lanesection_s0 + roadmark_group.s_offset;
            let s_start_roadmark_group = s_start.max(roadmark_group_s0.0);
            let s_end_roadmark_group = match roadmark_group_iter.peek() {
                None => s_end,
                Some(&next_roadmark) => {
                    s_end.min(next_roadmark.lanesection_s0.0 + next_roadmark.s_offset.0)
                }
            };
            if s_start_roadmark_group >= s_end_roadmark_group {
                continue;
            }
            let mut width = if roadmark_group.weight == "bold" {
                0.25
            } else {
                0.1
            };

            if roadmark_group.roadmakr_lines.is_empty() {
                if roadmark_group.width.0 > 0.0 {
                    width = roadmark_group.width.0;
                }
                roadmarks.push(RoadMark {
                    road_id: self.key.road_id.to_owned(),
                    lanesection_s0: self.key.lanesection_s0.0,
                    lane_id: self.id,
                    group_s0: roadmark_group_s0.0,
                    s_start: s_start_roadmark_group,
                    s_end: s_end_roadmark_group,
                    width,
                    mark_type: roadmark_group.mark_type.to_owned(),
                    t_offset: 0.0,
                });
            } else {
                for roadmark_line in &roadmark_group.roadmakr_lines {
                    if roadmark_line.width.0 > 0.0 {
                        width = roadmark_line.width.0;
                    }
                    if roadmark_line.length.0 + roadmark_line.space.0 == 0.0 {
                        continue;
                    }
                    let s0_roadmarks_line = roadmark_line.group_s0.0 + roadmark_line.s_offset.0;
                    let mut s_start_single_roadmark = s0_roadmarks_line;
                    while s_start_single_roadmark < s_end_roadmark_group {
                        let s_end_single_roadmark =
                            s_end.min(s_start_single_roadmark + roadmark_line.width.0);
                        roadmarks.push(RoadMark {
                            road_id: self.key.road_id.to_owned(),
                            lanesection_s0: self.key.lanesection_s0.0,
                            lane_id: self.id,
                            group_s0: roadmark_group_s0.0,
                            s_start: s_start_single_roadmark,
                            s_end: s_end_single_roadmark,
                            width,
                            t_offset: roadmark_line.t_offset.0,
                            mark_type: roadmark_group.mark_type.clone()
                                + roadmark_line.name.as_str(),
                        });

                        s_start_single_roadmark += roadmark_line.length.0 + roadmark_line.space.0;
                    }
                }
            }
        }
        roadmarks
    }
}
