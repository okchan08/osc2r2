use roxmltree;

use std::collections::HashMap;

use ordered_float::OrderedFloat;

use crate::open_drive::geometry::cubic_spline::{CubicSpline, Poly3};
use crate::open_drive::geometry::line::Line;
use crate::open_drive::geometry::road_geometry::GeometryType;
use crate::open_drive::ref_line::RefLine;

use super::lane_section::LaneSection;

#[derive(Debug)]
pub struct Road {
    length: f64,
    id: String,
    name: String,
    is_rht: bool,

    lane_offset: CubicSpline,
    superelevation: CubicSpline,

    s_to_lanesection: HashMap<OrderedFloat<f64>, LaneSection>,

    ref_line: RefLine,
}

impl Road {
    pub fn parse_roads(node: &mut roxmltree::Node) -> HashMap<String, Road> {
        let mut results = HashMap::new();
        for mut road_node in node.children() {
            if !road_node.has_tag_name("road") {
                continue;
            }
            let mut road_id = road_node.attribute("id").unwrap_or("").to_string();
            if results.contains_key(&road_id) {
                println!("road id {} already exists", road_id);
                road_id = road_id + "_duplicated";
            }
            let rule = road_node
                .attribute("rule")
                .unwrap_or("RHT")
                .to_string()
                .to_lowercase();
            let is_rht = rule == "rht";
            let length = road_node
                .attribute("length")
                .unwrap_or("0.0")
                .parse::<f64>()
                .unwrap();
            let mut road = Road {
                id: road_id.to_owned(),
                length: length,
                name: road_node.attribute("name").unwrap_or("").to_string(),
                is_rht: is_rht,
                lane_offset: CubicSpline::new(),
                superelevation: CubicSpline::new(),
                ref_line: RefLine::new(&road_id, &length),
                s_to_lanesection: HashMap::new(),
            };

            Self::parse_road_geometry(&mut road, &mut road_node);

            if let Some(lanes_node) = road_node
                .children()
                .find(|&node| node.has_tag_name("lanes"))
            {
                for mut lanesection_node in lanes_node
                    .children()
                    .filter(|&node| node.has_tag_name("laneSection"))
                {
                    let lanesection =
                        LaneSection::parse_lane_section(&mut lanesection_node, &road_id);
                    road.s_to_lanesection
                        .insert(OrderedFloat(lanesection.s0), lanesection);
                }
            }

            if road_id == "0" {
                println!("{:?}", road);
            }

            results.insert(road_id, road);
        }
        results
    }

    fn parse_road_geometry(road: &mut Road, road_node: &mut roxmltree::Node) {
        for node in road_node.descendants() {
            if !node.has_tag_name("planView") {
                continue;
            }

            for geometry_hdr_node in node.children() {
                if !geometry_hdr_node.has_tag_name("geometry") {
                    continue;
                }
                let s0 = geometry_hdr_node
                    .attribute("s")
                    .unwrap_or("0.0")
                    .parse::<f64>()
                    .unwrap();
                let x0 = geometry_hdr_node
                    .attribute("x")
                    .unwrap_or("0.0")
                    .parse::<f64>()
                    .unwrap();
                let y0 = geometry_hdr_node
                    .attribute("y")
                    .unwrap_or("0.0")
                    .parse::<f64>()
                    .unwrap();
                let hdg0 = geometry_hdr_node
                    .attribute("hdg")
                    .unwrap_or("0.0")
                    .parse::<f64>()
                    .unwrap();
                let length = geometry_hdr_node
                    .attribute("length")
                    .unwrap_or("0.0")
                    .parse::<f64>()
                    .unwrap();

                let geometry_node = geometry_hdr_node
                    .children()
                    .into_iter()
                    .find(|&node| node.tag_name().name() != "")
                    .unwrap();
                let geometry_type = geometry_node
                    .tag_name()
                    .name()
                    .parse::<GeometryType>()
                    .unwrap();

                match geometry_type {
                    GeometryType::GeometryTypeLine => road
                        .ref_line
                        .append_road_geometry(s0, Box::new(Line::new(s0, x0, y0, hdg0, length))),
                    _ => {}
                }

                Self::parse_road_reference_line(road, road_node);
            }
        }
    }

    fn parse_road_reference_line(road: &mut Road, road_node: &mut roxmltree::Node) {
        let elevation_profile_node = road_node
            .children()
            .into_iter()
            .find(|&node| node.has_tag_name("elevationProfile"))
            .unwrap();
        // Not all splines starting at s = 0 so that we assume value 0.0 until
        // explicitly specified in OpenDRIVE file
        road.ref_line
            .get_elevation_profile_mut()
            .append_poly3(0.0, Poly3::new(0.0, 0.0, 0.0, 0.0));

        for elevation_node in elevation_profile_node.children() {
            let s0 = elevation_node
                .attribute("s")
                .unwrap_or("0.0")
                .parse()
                .unwrap();
            let a = elevation_node
                .attribute("a")
                .unwrap_or("0.0")
                .parse()
                .unwrap();
            let b = elevation_node
                .attribute("b")
                .unwrap_or("0.0")
                .parse()
                .unwrap();
            let c = elevation_node
                .attribute("c")
                .unwrap_or("0.0")
                .parse()
                .unwrap();
            let d = elevation_node
                .attribute("d")
                .unwrap_or("0.0")
                .parse()
                .unwrap();
            road.ref_line
                .get_elevation_profile_mut()
                .append_poly3(s0, Poly3::new(a, b, c, d));
        }
    }
}
