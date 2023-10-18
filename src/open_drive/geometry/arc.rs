use super::road_geometry::{GeometryType, RoadGeometry};
use crate::open_drive::math::Vec2;
use std::f64::consts::PI;

#[derive(Clone, Copy, Debug)]
pub struct Arc {
    s0: f64,
    x0: f64,
    y0: f64,
    hdg0: f64,
    length: f64,
    curvature: f64,
}

impl Arc {
    pub fn new(s0: f64, x0: f64, y0: f64, hdg0: f64, length: f64, curvature: f64) -> Self {
        Self {
            s0,
            x0,
            y0,
            hdg0,
            length,
            curvature,
        }
    }
}

impl RoadGeometry for Arc {
    fn get_xy(&self, s: f64) -> Vec2 {
        let angle_at_s = (s - self.s0) * self.curvature - PI / 2.0;
        let r = 1.0 / self.curvature;
        let xs = r * ((self.hdg0 + angle_at_s).cos() - self.hdg0.sin()) + self.x0;
        let ys = r * ((self.hdg0 + angle_at_s).sin() + self.hdg0.cos()) + self.y0;
        Vec2(xs, ys)
    }

    fn get_grad(&self, s: f64) -> Vec2 {
        let dx = ((PI / 2.0) - self.curvature * (s - self.s0) - self.hdg0).sin();
        let dy = ((PI / 2.0) - self.curvature * (s - self.s0) - self.hdg0).cos();
        Vec2(dx, dy)
    }

    fn approximate_linear(&self, _eps: f64) -> Vec<f64> {
        // TODO: properly implement
        let mut retval = vec![];
        let s_step = 0.01 / self.curvature.abs(); // sample at approx. every 1 deg.
        let mut curr_s = self.s0;
        while curr_s < (self.s0 + self.length) {
            retval.push(curr_s);
            curr_s += s_step;
        }
        retval.push(self.s0 + self.length);
        retval
    }

    fn geometry_type(&self) -> GeometryType {
        GeometryType::Arc
    }
}
