use crate::open_drive::math::Vec2;

use std::str::FromStr;
use std::vec::Vec;

pub enum GeometryType {
    Line,
    Spiral,
    Arc,
    ParamPoly3,
}

impl FromStr for GeometryType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "line" => Ok(GeometryType::Line),
            "arc" => Ok(GeometryType::Arc),
            "spiral" => Ok(GeometryType::Spiral),
            "paramPoly3" => Ok(GeometryType::ParamPoly3),
            _ => Err(format!("undefined geometry type: {}", s)),
        }
    }
}

impl ToString for GeometryType {
    fn to_string(&self) -> String {
        match self {
            GeometryType::Arc => "arc".to_string(),
            GeometryType::Line => "line".to_string(),
            GeometryType::Spiral => "spiral".to_string(),
            GeometryType::ParamPoly3 => "paramPoly3".to_string(),
        }
    }
}

pub trait RoadGeometry {
    fn get_xy(&self, s: f64) -> Vec2;
    fn get_grad(&self, s: f64) -> Vec2;
    fn approximate_linear(&self, eps: f64) -> Vec<f64>;
    fn geometry_type(&self) -> GeometryType;
}

impl std::fmt::Debug for dyn RoadGeometry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RoadGeometry{{{}}}", self.geometry_type().to_string())
    }
}
