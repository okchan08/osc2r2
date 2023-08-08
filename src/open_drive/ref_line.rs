use crate::open_drive::geometry::cubic_spline::CubicSpline;
use crate::open_drive::geometry::road_geometry::RoadGeometry;

use ordered_float::OrderedFloat;

use std::collections::HashMap;

#[derive(Debug)]
pub struct RefLine {
    road_id: String,
    length: f64,
    elevation_profile: CubicSpline,
    s0_to_geometry: HashMap<OrderedFloat<f64>, Box<dyn RoadGeometry>>,
}

impl RefLine {
    pub fn new(road_id: &String, length: &f64) -> Self {
        Self {
            road_id: road_id.to_owned(),
            length: length.to_owned(),
            elevation_profile: CubicSpline::new(),
            s0_to_geometry: HashMap::new(),
        }
    }

    pub fn append_road_geometry(&mut self, key: f64, geometry: Box<dyn RoadGeometry>) {
        self.s0_to_geometry.insert(OrderedFloat(key), geometry);
    }

    pub fn get_elevation_profile_mut(&mut self) -> &mut CubicSpline {
        &mut self.elevation_profile
    }
}
