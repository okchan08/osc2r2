use crate::open_drive::geometry::cubic_spline::CubicSpline;
use crate::open_drive::geometry::road_geometry::RoadGeometry;

use ordered_float::OrderedFloat;

use std::collections::{BTreeMap, BTreeSet};

use super::{math::Vec2, math::Vec3};

#[derive(Debug)]
pub struct RefLine {
    pub road_id: String,
    pub length: f64,
    pub elevation_profile: CubicSpline,
    pub s0_to_geometry: BTreeMap<OrderedFloat<f64>, Box<dyn RoadGeometry>>,
}

impl RefLine {
    pub fn new(road_id: &String, length: &f64) -> Self {
        Self {
            road_id: road_id.to_owned(),
            length: length.to_owned(),
            elevation_profile: CubicSpline::new(),
            s0_to_geometry: BTreeMap::new(),
        }
    }

    pub fn append_road_geometry(&mut self, key: f64, geometry: Box<dyn RoadGeometry>) {
        self.s0_to_geometry.insert(OrderedFloat(key), geometry);
    }

    pub fn approximate_linear(
        &self,
        eps: f64,
        s_start: f64,
        s_end: f64,
    ) -> BTreeSet<OrderedFloat<f64>> {
        if s_start == s_end || self.s0_to_geometry.is_empty() {
            return BTreeSet::new();
        }

        let s0_geom_range = self
            .s0_to_geometry
            .range(OrderedFloat(s_start)..OrderedFloat(s_end));

        let mut s_vals = BTreeSet::new();
        s_vals.insert(OrderedFloat(s_start));

        for s0_geom in s0_geom_range {
            let s_vals_geom = s0_geom.1.approximate_linear(eps);

            if s_vals_geom.len() < 2 {
                panic!("expected at least two sampled points");
            }

            for s_val in s_vals_geom.iter() {
                if s_start < *s_val && *s_val < s_end {
                    s_vals.insert(OrderedFloat(*s_val));
                }
            }
            // TODO handle pop last
        }

        let s_vals_elevation = self
            .elevation_profile
            .approximate_linear(eps, s_start, s_end);

        for s_val in s_vals_elevation
            .iter()
            .filter(|&&s| s_start < s && s < s_end)
        {
            s_vals.insert(OrderedFloat(*s_val));
        }
        s_vals.insert(OrderedFloat(s_end));
        s_vals
    }

    pub fn get_geometry_s0(&self, s: f64) -> f64 {
        if self.s0_to_geometry.is_empty() {
            return f64::NAN;
        }

        self.s0_to_geometry
            .range(..OrderedFloat(s))
            .next_back()
            .unwrap_or(self.s0_to_geometry.first_key_value().unwrap())
            .0
             .0
    }

    pub fn get_geometry(&self, s: f64) -> Option<&Box<dyn RoadGeometry>> {
        let geom_s0 = self.get_geometry_s0(s);
        if geom_s0.is_nan() {
            return None;
        }
        Some(self.s0_to_geometry.get(&OrderedFloat(geom_s0)).unwrap())
    }

    pub fn get_grad(&self, s: f64) -> Vec3 {
        let geom = self.get_geometry(s);
        let pt_grad = match geom {
            Some(g) => g.get_grad(s),
            None => Vec2::new(0.0, 0.0),
        };
        // TODO handle height here.
        Vec3::new(pt_grad.0, pt_grad.1, 0.0)
    }

    pub fn get_xyz(&self, s: f64) -> Vec3 {
        let geom = self.get_geometry(s);
        let pt_xy = match geom {
            Some(g) => g.get_xy(s),
            None => Vec2::new(0.0, 0.0),
        };
        // TODO handle height here.
        Vec3::new(pt_xy.0, pt_xy.1, 0.0)
    }
}
