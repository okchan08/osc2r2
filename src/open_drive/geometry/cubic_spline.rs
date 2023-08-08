#![feature(btree_cursors)]

use std::collections::{BTreeMap, BTreeSet};
use std::ops::Bound;

use ordered_float::OrderedFloat;

#[derive(Debug, Clone, Copy)]
pub struct Poly3 {
    a: f64,
    b: f64,
    c: f64,
    d: f64,
}

impl Poly3 {
    pub fn new(a: f64, b: f64, c: f64, d: f64) -> Self {
        Poly3 { a, b, c, d }
    }

    pub fn is_nan(&self) -> bool {
        f64::is_nan(self.a) || f64::is_nan(self.b) || f64::is_nan(self.c) || f64::is_nan(self.d)
    }

    pub fn negate(&mut self) {
        self.a = -self.a;
        self.b = -self.b;
        self.c = -self.c;
        self.d = -self.d;
    }
}

#[derive(Debug, Clone)]
pub struct CubicSpline {
    s0_to_poly: BTreeMap<OrderedFloat<f64>, Poly3>,
}

impl CubicSpline {
    pub fn new() -> Self {
        CubicSpline {
            s0_to_poly: BTreeMap::new(),
        }
    }

    pub fn add(&self, other: &CubicSpline) -> Self {
        if self.s0_to_poly.is_empty() {
            return other.clone();
        }
        if other.s0_to_poly.is_empty() {
            return self.clone();
        }

        let mut s0_vals = self
            .s0_to_poly
            .keys()
            .into_iter()
            .map(|&key| key.clone())
            .collect::<BTreeSet<OrderedFloat<f64>>>();
        let mut other_s0 = other
            .s0_to_poly
            .keys()
            .into_iter()
            .map(|&key| key.clone())
            .collect::<BTreeSet<OrderedFloat<f64>>>();
        s0_vals.append(&mut other_s0);

        let mut retval = Self::new();

        for s0 in s0_vals.into_iter() {
            let this_poly = self.get_poly(s0.0, false);
            let other_poly = other.get_poly(s0.0, false);

            if this_poly.is_nan() || other_poly.is_nan() {
                retval.s0_to_poly.insert(
                    s0,
                    if this_poly.is_nan() {
                        other_poly
                    } else {
                        this_poly
                    },
                );
            } else {
                retval.s0_to_poly.insert(
                    s0,
                    Poly3::new(
                        this_poly.a + other_poly.a,
                        this_poly.b + other_poly.b,
                        this_poly.c + other_poly.c,
                        this_poly.d + other_poly.d,
                    ),
                );
            }
        }
        retval
    }

    pub fn append_poly3(&mut self, s: f64, poly3: Poly3) {
        self.s0_to_poly.insert(OrderedFloat(s), poly3);
    }

    pub fn negate(&self) -> CubicSpline {
        let mut ret = self.clone();

        for (_, poly) in ret.s0_to_poly.iter_mut() {
            poly.negate();
        }

        ret
    }

    pub fn get_poly(&self, s: f64, extend_start: bool) -> Poly3 {
        let nan_poly = Poly3::new(f64::NAN, f64::NAN, f64::NAN, f64::NAN);
        if self.s0_to_poly.is_empty() {
            return nan_poly;
        }

        if extend_start == false && s < self.s0_to_poly.first_key_value().unwrap().0 .0 {
            return nan_poly;
        }

        let (_, target_poly) = self
            .s0_to_poly
            .range(..OrderedFloat(s))
            .rev()
            .next()
            .unwrap_or((&OrderedFloat(f64::NAN), &nan_poly));

        target_poly.clone()

        // TODO might be wrong here

        //auto target_poly_iter = this->s0_to_poly.upper_bound(s);
        //if (target_poly_iter != this->s0_to_poly.begin())
        //    target_poly_iter--;
        //return target_poly_iter->second;
    }
}
