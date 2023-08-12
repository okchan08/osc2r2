use roxmltree;

use std::collections::{BTreeMap, BTreeSet, HashMap};

use ordered_float::OrderedFloat;

use crate::open_drive::geometry::arc::Arc;
use crate::open_drive::geometry::cubic_spline::{CubicSpline, Poly3};
use crate::open_drive::geometry::line::Line;
use crate::open_drive::geometry::road_geometry::GeometryType;
use crate::open_drive::math::Mat3D;
use crate::open_drive::ref_line::RefLine;

use super::geometry::spiral::Spiral;
use super::lane::Lane;
use super::lane_section::LaneSection;
use super::math::{Vec2, Vec3};
use super::mesh::Mesh3D;
use super::road_mark::RoadMark;

#[derive(Debug)]
pub struct Road {
    pub length: f64,
    pub id: String,
    pub name: String,
    pub is_rht: bool,

    pub lane_offset: CubicSpline,
    pub superelevation: CubicSpline,

    pub s_to_lanesection: BTreeMap<OrderedFloat<f64>, LaneSection>,

    pub ref_line: RefLine,
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
                s_to_lanesection: BTreeMap::new(),
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
                    let lanesection = LaneSection::parse_lane_section(&mut lanesection_node, &road);
                    road.s_to_lanesection
                        .insert(OrderedFloat(lanesection.s0), lanesection);
                }
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
                    GeometryType::GeometryTypeArc => {
                        let curvature = geometry_node
                            .attribute("curvature")
                            .unwrap()
                            .parse()
                            .unwrap();
                        road.ref_line.append_road_geometry(
                            s0,
                            Box::new(Arc::new(s0, x0, y0, hdg0, length, curvature)),
                        );
                    }
                    GeometryType::GeometryTypeSpiral => {
                        let curv_start = geometry_node
                            .attribute("curvStart")
                            .unwrap_or("0.0")
                            .parse()
                            .unwrap();
                        let curv_end = geometry_node
                            .attribute("curvEnd")
                            .unwrap_or("0.0")
                            .parse()
                            .unwrap();
                        road.ref_line.append_road_geometry(
                            s0,
                            Box::new(Spiral::new(s0, x0, y0, hdg0, length, curv_start, curv_end)),
                        );
                    }
                    _ => {}
                }

                Self::parse_road_reference_line(road, road_node);
            }
        }
        for s in road.ref_line.s0_to_geometry.keys().into_iter() {
            let geom = road.ref_line.s0_to_geometry.get(&s).unwrap();
        }
    }

    fn parse_road_reference_line(road: &mut Road, road_node: &mut roxmltree::Node) {
        let elevation_profile_node = road_node
            .children()
            .find(|&node| node.has_tag_name("elevationProfile"))
            .unwrap();

        let laneoffset_node = road_node
            .children()
            .find(|&node| node.has_tag_name("lanes"))
            .unwrap();
        // Not all splines starting at s = 0 so that we assume value 0.0 until
        // explicitly specified in OpenDRIVE file
        road.ref_line
            .elevation_profile
            .append_poly3(0.0, Poly3::new(0.0, 0.0, 0.0, 0.0));

        for elevation_node in elevation_profile_node.children() {
            if !elevation_node.has_tag_name("elevation") {
                continue;
            }
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
                .elevation_profile
                .append_poly3(s0, Poly3::new(a, b, c, d));
        }
        road.lane_offset
            .append_poly3(0.0, Poly3::new(0.0, 0.0, 0.0, 0.0));
        for laneoffset_node in laneoffset_node.children() {
            if !laneoffset_node.has_tag_name("laneOffset") {
                continue;
            }
            let s0 = laneoffset_node
                .attribute("s")
                .unwrap_or("0.0")
                .parse()
                .unwrap();
            let a = laneoffset_node
                .attribute("a")
                .unwrap_or("0.0")
                .parse()
                .unwrap();
            let b = laneoffset_node
                .attribute("b")
                .unwrap_or("0.0")
                .parse()
                .unwrap();
            let c = laneoffset_node
                .attribute("c")
                .unwrap_or("0.0")
                .parse()
                .unwrap();
            let d = laneoffset_node
                .attribute("d")
                .unwrap_or("0.0")
                .parse()
                .unwrap();
            road.lane_offset.append_poly3(s0, Poly3::new(a, b, c, d));
        }
    }

    pub fn get_lane_mesh(&self, lane: &Lane, eps: f64) -> Mesh3D {
        let s_end = self.get_lanesection_end(lane.key.lanesection_s0);
        self.get_lane_mesh_in_s_range(lane, lane.key.lanesection_s0, s_end, eps)
    }

    fn get_lane_mesh_in_s_range(&self, lane: &Lane, s_start: f64, s_end: f64, eps: f64) -> Mesh3D {
        let mut s_vals = self.ref_line.approximate_linear(eps, s_start, s_end);
        let s_vals_outer_brdr = lane.outer_border.approximate_linear(eps, s_start, s_end);
        s_vals.extend(s_vals_outer_brdr.into_iter().map(|val| OrderedFloat(val)));
        let s_vals_inner_brdr = lane.inner_border.approximate_linear(eps, s_start, s_end);
        s_vals.extend(s_vals_inner_brdr.into_iter().map(|val| OrderedFloat(val)));
        let s_vals_lane_offset = self.lane_offset.approximate_linear(eps, s_start, s_end);
        s_vals.extend(s_vals_lane_offset.into_iter().map(|val| OrderedFloat(val)));

        // TODO s vals for height
        // TODO s vals for super elevation
        // TODO clean up btree set

        let mut out_mesh = Mesh3D::new();

        for s_val in s_vals.iter() {
            let mut vn_inner_brdr = Vec3::new(0.0, 0.0, 0.0);
            let t_inner_brdr = lane.inner_border.get(s_val.0, 0.0, true);
            out_mesh
                .vertices
                .push(self.get_surface_pt(s_val.0, t_inner_brdr, &mut vn_inner_brdr));
            out_mesh.normals.push(vn_inner_brdr);
            out_mesh
                .st_coordinates
                .push(Vec2::new(s_val.0, t_inner_brdr));

            let mut vn_outer_brdr = Vec3::new(0.0, 0.0, 0.0);
            let t_outer_brdr = lane.outer_border.get(s_val.0, 0.0, true);
            out_mesh
                .vertices
                .push(self.get_surface_pt(s_val.0, t_outer_brdr, &mut vn_outer_brdr));
            out_mesh.normals.push(vn_outer_brdr);
            out_mesh
                .st_coordinates
                .push(Vec2::new(s_val.0, t_outer_brdr));
        }

        let num_pts = out_mesh.vertices.len();
        let ccw = lane.id > 0;
        for i in (3..num_pts).step_by(2) {
            let idx = i as u32;
            let indicies_patch = if ccw {
                vec![idx - 3, idx - 1, idx, idx - 3, idx, idx - 2]
            } else {
                vec![idx - 3, idx, idx - 1, idx - 3, idx - 2, idx]
            };
            out_mesh.indicies.extend(indicies_patch);
        }
        out_mesh
    }

    pub fn get_lanesection_end(&self, lanesection_s0: f64) -> f64 {
        let s_lanesec = self.s_to_lanesection.get(&OrderedFloat(lanesection_s0));
        if s_lanesec.is_none() {
            return f64::NAN;
        }

        // if obtained lane section is the previous of the last one, return the road length.
        if &OrderedFloat(s_lanesec.unwrap().s0)
            == self.s_to_lanesection.iter().rev().next().unwrap().0
        {
            return self.length;
        }

        match self
            .s_to_lanesection
            .iter()
            .find(|(s0, _)| *s0 > &OrderedFloat(lanesection_s0))
        {
            Some((s0, _)) => s0.0,
            None => f64::NAN,
        }
    }

    fn get_surface_pt(&self, s: f64, t: f64, vn: &mut Vec3) -> Vec3 {
        let lanesection_s0 = self.get_lanesection_s0(s);
        if lanesection_s0.is_nan() {
            panic!(
                "cannot get road surface pt, no lane section for s = {}, road length = {}",
                s, self.length
            );
        }
        let lanesection = self
            .s_to_lanesection
            .get(&OrderedFloat(lanesection_s0))
            .unwrap();
        let lane = lanesection
            .id_to_lane
            .get(&lanesection.get_lane_id(s, t))
            .unwrap();
        let t_inner_brdr = lane.inner_border.get(s, 0.0, true);

        // TODO handle height here.
        let h_t = 0.0;

        self.get_xyz(s, t, h_t, Some(vn))
    }

    fn get_lanesection_s0(&self, s: f64) -> f64 {
        if self.s_to_lanesection.is_empty() {
            return f64::NAN;
        }

        let lanesection = self
            .s_to_lanesection
            .range(..OrderedFloat(s))
            .next_back()
            .unwrap_or(self.s_to_lanesection.first_key_value().unwrap());
        if lanesection.1.s0 > s || s > self.get_lanesection_end(lanesection.1.s0) {
            return f64::NAN;
        }
        lanesection.1.s0
    }

    pub fn get_roadmark_mesh(&self, lane: &Lane, roadmark: &RoadMark, eps: f64) -> Mesh3D {
        let s_vals =
            self.approximate_lane_border_linear(lane, roadmark.s_start, roadmark.s_end, eps, true);
        let mut out_mesh = Mesh3D::new();

        for s_val in s_vals {
            let mut vn_edge_a = Vec3::new(0.0, 0.0, 0.0);
            let t_edge_a = lane.outer_border.get(s_val.0, 0.0, true)
                + roadmark.width * 0.5
                + roadmark.t_offset;
            out_mesh
                .vertices
                .push(self.get_surface_pt(s_val.0, t_edge_a, &mut vn_edge_a));
            out_mesh.normals.push(vn_edge_a);

            let mut vn_edge_b = Vec3::new(0.0, 0.0, 0.0);
            let t_edge_b = t_edge_a - roadmark.width;
            out_mesh
                .vertices
                .push(self.get_surface_pt(s_val.0, t_edge_b, &mut vn_edge_b));
            out_mesh.normals.push(vn_edge_b);
        }

        let num_pts = out_mesh.vertices.len();
        for i in (3..num_pts).step_by(2) {
            let idx = i as u32;
            out_mesh
                .indicies
                .extend(vec![idx - 3, idx, idx - 1, idx - 3, idx - 2, idx]);
        }
        out_mesh
    }

    fn approximate_lane_border_linear(
        &self,
        lane: &Lane,
        s_start: f64,
        s_end: f64,
        eps: f64,
        outer: bool,
    ) -> BTreeSet<OrderedFloat<f64>> {
        let mut s_vals = self.ref_line.approximate_linear(eps, s_start, s_end);
        let border = if outer {
            &lane.outer_border
        } else {
            &lane.inner_border
        };
        let s_vals_brdr = border.approximate_linear(eps, s_start, s_end);
        s_vals.append(
            &mut s_vals_brdr
                .into_iter()
                .map(|val| OrderedFloat(val))
                .collect(),
        );
        // TODO handle height and superelevation here
        s_vals
    }

    fn get_xyz(&self, s: f64, t: f64, h: f64, _e_h: Option<&mut Vec3>) -> Vec3 {
        let s_vec = self.ref_line.get_grad(s);
        let theta: f64 = 0.0; // TODO handle superelevation here

        let e_s = s_vec.normalize();
        let e_t = Vec3::new(
            theta.cos() * (-e_s.1) + theta.sin() * (-e_s.2) * e_s.0,
            theta.cos() * e_s.0 + theta.sin() * (-e_s.2) * e_s.1,
            theta.sin() * (e_s.0 * e_s.0 + e_s.1 * e_s.1),
        );

        let e_h = Vec3::cross_produc(&s_vec, &e_t).normalize();
        let p0 = self.ref_line.get_xyz(s);

        let trans_mat = Mat3D::new(
            Vec3::new(e_t.0, e_h.0, p0.0),
            Vec3::new(e_t.1, e_h.1, p0.1),
            Vec3::new(e_t.2, e_h.2, p0.2),
        );
        if let Some(eh) = _e_h {
            *eh = e_h.clone();
        }
        Mat3D::mat_vec_multiplication(&trans_mat, &Vec3::new(t, h, 1.0))
    }
}
