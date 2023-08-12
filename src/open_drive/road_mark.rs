use crate::open_drive::utils::roxmltree_util::parse_node_attribute;
use ordered_float::OrderedFloat;
use roxmltree;
use std::collections::BTreeSet;

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct RoadMarkLine {
    pub road_id: String,
    pub lanesection_s0: OrderedFloat<f64>,
    pub lane_id: i32,
    pub group_s0: OrderedFloat<f64>,
    pub width: OrderedFloat<f64>,
    pub length: OrderedFloat<f64>,
    pub space: OrderedFloat<f64>,
    pub t_offset: OrderedFloat<f64>,
    pub s_offset: OrderedFloat<f64>,

    pub name: String,
    pub rule: String,
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct RoadMarkGroup {
    pub road_id: String,
    pub lanesection_s0: OrderedFloat<f64>,
    pub lane_id: i32,
    pub width: OrderedFloat<f64>,
    pub height: OrderedFloat<f64>,
    pub s_offset: OrderedFloat<f64>,
    pub mark_type: String,
    pub weight: String,
    pub color: String,
    pub material: String,
    pub lane_change: String,
    pub roadmakr_lines: BTreeSet<RoadMarkLine>,
}

#[derive(Debug)]
pub struct RoadMark {
    pub road_id: String,
    pub lanesection_s0: f64,
    pub lane_id: i32,
    pub group_s0: f64,
    pub s_start: f64,
    pub s_end: f64,
    pub t_offset: f64,
    pub width: f64,

    pub mark_type: String,
}

impl RoadMarkGroup {
    pub fn parse_road_mark_group(
        roadmark_node: &roxmltree::Node,
        lane_id: i32,
        lanesection_s0: f64,
        road_id: &String,
    ) -> RoadMarkGroup {
        let mut roadmark_group = RoadMarkGroup {
            road_id: road_id.to_owned(),
            lanesection_s0: OrderedFloat(lanesection_s0),
            lane_id: lane_id,
            width: OrderedFloat(parse_node_attribute(roadmark_node, "width", -1.0)),
            height: OrderedFloat(parse_node_attribute(roadmark_node, "height", 0.0)),
            s_offset: OrderedFloat(parse_node_attribute(roadmark_node, "sOffset", 0.0)),
            mark_type: parse_node_attribute(roadmark_node, "type", "none".to_string()),
            weight: parse_node_attribute(roadmark_node, "weight", "standard".to_string()),
            color: parse_node_attribute(roadmark_node, "color", "standard".to_string()),
            material: parse_node_attribute(roadmark_node, "material", "standard".to_string()),
            lane_change: parse_node_attribute(roadmark_node, "laneChange", "both".to_string()),
            roadmakr_lines: BTreeSet::new(),
        };
        let roadmark_group_s0 = lanesection_s0 + roadmark_group.s_offset.0;

        for roadmark_type_node in roadmark_node
            .children()
            .filter(|&node| node.has_tag_name("type"))
        {
            let name = parse_node_attribute(&roadmark_type_node, "name", "".to_string());
            let line_width_1 = parse_node_attribute(&roadmark_type_node, "width", -1.0);

            for roadmark_line_node in roadmark_type_node
                .children()
                .filter(|&node| node.has_tag_name("line"))
            {
                let line_width_0 = parse_node_attribute(&roadmark_line_node, "width", -1.0);
                let roadmark_width = if line_width_0 < 0.0 {
                    line_width_1
                } else {
                    line_width_0
                };

                let roadmark_line = RoadMarkLine {
                    road_id: road_id.to_owned(),
                    lanesection_s0: OrderedFloat(lanesection_s0),
                    lane_id: lane_id,
                    group_s0: OrderedFloat(roadmark_group_s0),
                    width: OrderedFloat(roadmark_width),
                    length: OrderedFloat(parse_node_attribute(&roadmark_line_node, "length", 0.0)),
                    space: OrderedFloat(parse_node_attribute(&roadmark_line_node, "space", 0.0)),
                    t_offset: OrderedFloat(parse_node_attribute(
                        &roadmark_line_node,
                        "tOffset",
                        0.0,
                    )),
                    s_offset: OrderedFloat(parse_node_attribute(
                        &roadmark_line_node,
                        "sOffset",
                        0.0,
                    )),
                    name: name.to_owned(),
                    rule: parse_node_attribute(&roadmark_line_node, "rule", "none".to_string()),
                };
                roadmark_group.roadmakr_lines.insert(roadmark_line);
            }
        }
        roadmark_group
    }
}
