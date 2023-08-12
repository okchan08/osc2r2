use super::road_geometry::{GeometryType, RoadGeometry};
use crate::open_drive::math::Vec2;

use std::vec::Vec;

#[derive(Debug)]
pub struct Line {
    s0: f64,
    x0: f64,
    y0: f64,
    hdg0: f64,
    length: f64,
}

impl Line {
    pub fn new(s0: f64, x0: f64, y0: f64, hdg0: f64, length: f64) -> Self {
        Line {
            s0,
            x0,
            y0,
            hdg0,
            length,
        }
    }
}

impl RoadGeometry for Line {
    fn get_xy(&self, s: f64) -> Vec2 {
        let x = self.hdg0.cos() * (s - self.s0) + self.x0;
        let y = self.hdg0.sin() * (s - self.s0) + self.y0;
        Vec2::new(x, y)
    }

    fn get_grad(&self, _s: f64) -> Vec2 {
        Vec2::new(self.hdg0.cos(), self.hdg0.sin())
    }

    fn approximate_linear(&self, _eps: f64) -> Vec<f64> {
        vec![self.s0, self.s0 + self.length]
    }

    fn geometry_type(&self) -> GeometryType {
        GeometryType::GeometryTypeLine
    }
}
