#[derive(Debug, Clone, Copy)]
pub struct Vec2(pub f64, pub f64);

impl Vec2 {
    pub fn new(x: f64, y: f64) -> Self {
        Self(x, y)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Vec3(pub f64, pub f64, pub f64);

impl Vec3 {
    pub fn new(x: f64, y: f64, z: f64) -> Self {
        Self(x, y, z)
    }

    pub fn normalize(&self) -> Self {
        let norm = (self.0 * self.0 + self.1 * self.1 + self.2 * self.2).sqrt();
        Self(self.0 / norm, self.1 / norm, self.2 / norm)
    }

    pub fn cross_produc(a: &Self, b: &Self) -> Self {
        Self(
            a.1 * b.2 - a.2 * b.1,
            a.2 * b.0 - a.0 * b.2,
            a.0 * b.1 - a.1 * b.0,
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Mat3D(pub Vec3, pub Vec3, pub Vec3);

impl Mat3D {
    pub fn new(v1: Vec3, v2: Vec3, v3: Vec3) -> Self {
        Self(v1, v2, v3)
    }

    pub fn mat_vec_multiplication(mat: &Mat3D, v: &Vec3) -> Vec3 {
        let mut res = Vec3::new(0.0, 0.0, 0.0);

        res.0 = mat.0 .0 * v.0 + mat.0 .1 * v.1 + mat.0 .2 * v.2;
        res.1 = mat.1 .0 * v.0 + mat.1 .1 * v.1 + mat.1 .2 * v.2;
        res.2 = mat.2 .0 * v.0 + mat.2 .1 * v.1 + mat.2 .2 * v.2;

        res
    }
}
