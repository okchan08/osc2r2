use super::math::Vec3;
pub struct Transform {
    position: Vec3,
    direction: Vec3,
}

impl Transform {
    pub fn new(position: Vec3, direction: Vec3) -> Self {
        Self {
            position: position,
            direction: direction,
        }
    }

    pub fn position(&self) -> Vec3 {
        self.position
    }

    pub fn direction(&self) -> Vec3 {
        self.direction
    }
}
