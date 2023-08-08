use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct ApproxF64(f64);

impl ApproxF64 {
    pub fn new(val: f64) -> Self {
        ApproxF64(val)
    }
}

impl Eq for ApproxF64 {}

impl Hash for ApproxF64 {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.0.to_bits().hash(state)
    }
}

#[derive(Debug)]
pub struct Vec2(ApproxF64, ApproxF64);

impl Vec2 {
    pub fn new(x: f64, y: f64) -> Self {
        Self(ApproxF64(x), ApproxF64(y))
    }
}

#[derive(Debug)]
pub struct Vec3(f64, f64, f64);
