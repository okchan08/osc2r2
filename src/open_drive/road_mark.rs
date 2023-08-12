use crate::open_drive::utils::roxmltree_util::parse_node_attribute;
use ordered_float::OrderedFloat;
use roxmltree;
use std::collections::BTreeSet;

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct RoadMarkLine {
    road_id: String,
    lanesection_s0: OrderedFloat<f64>,
    lane_id: i32,
    group_s0: OrderedFloat<f64>,
    width: OrderedFloat<f64>,
    length: OrderedFloat<f64>,
    space: OrderedFloat<f64>,
    t_offset: OrderedFloat<f64>,
    s_offset: OrderedFloat<f64>,

    name: String,
    rule: String,
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct RoadMarkGroup {
    road_id: String,
    lanesection_s0: OrderedFloat<f64>,
    lane_id: i32,
    width: OrderedFloat<f64>,
    height: OrderedFloat<f64>,
    s_offset: OrderedFloat<f64>,
    mark_type: String,
    weight: String,
    color: String,
    material: String,
    lane_change: String,
    roadmakr_lines: BTreeSet<RoadMarkLine>,
}

#[derive(Debug)]
pub struct RoadMark {
    road_id: String,
    lanesection_s0: f64,
    lane_id: i32,
    group_s0: f64,
    s_start: f64,
    s_end: f64,
    t_offset: f64,
    width: f64,

    mark_type: String,
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
