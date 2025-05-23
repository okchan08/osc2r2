use std::collections::{BTreeMap, BTreeSet};

use ordered_float::OrderedFloat;

use crate::open_drive::utils::cubic_bezier::CubicBezier;

#[derive(Debug, Clone, Copy)]
pub struct Poly3 {
    a: f64,
    b: f64,
    c: f64,
    d: f64,
}

impl Poly3 {
    pub fn new(s0: f64, a: f64, b: f64, c: f64, d: f64) -> Self {
        /* ds = s - s0 => resolve to polynomial form */
        /* make poly3s work on absolute s position => makes CubicSpline::add work */
        Poly3 {
            a: a - b * s0 + c * s0 * s0 - d * s0 * s0 * s0,
            b: b - 2.0 * c * s0 + 3.0 * d * s0 * s0,
            c: c - 3.0 * d * s0,
            d,
        }
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

    pub fn approximate_linear(&self, eps: f64, s_start: f64, s_end: f64) -> Vec<f64> {
        if s_start == s_end {
            return vec![];
        }

        if self.d == 0.0 && self.c == 0.0 {
            return vec![s_start, s_end];
        }

        let mut s_vals = vec![];

        if self.d == 0.0 && self.c != 0.0 {
            let mut s = s_start;
            while s < s_end {
                s_vals.push(s);
                s += f64::abs(eps / self.c).sqrt();
            }
        } else {
            let a = self.a;
            let b = self.b;
            let c = self.c;
            let d = self.d;
            /* transform to parametric form */
            let s_0 = s_start;
            let s_1 = s_end;
            let d_p = -d * s_0 * s_0 * s_0 + d * s_1 * s_1 * s_1 - 3.0 * d * s_0 * s_1 * s_1
                + 3.0 * d * s_0 * s_0 * s_1;
            let c_p = 3.0 * d * s_0 * s_0 * s_0 + 3.0 * d * s_0 * s_1 * s_1
                - 6.0 * d * s_0 * s_0 * s_1
                + c * s_0 * s_0
                + c * s_1 * s_1
                - 2.0 * c * s_0 * s_1;
            let b_p = -3.0 * d * s_0 * s_0 * s_0 + 3.0 * d * s_0 * s_0 * s_1 - 2.0 * c * s_0 * s_0
                + 2.0 * c * s_0 * s_1
                - b * s_0
                + b * s_1;
            let a_p = d * s_0 * s_0 * s_0 + c * s_0 * s_0 + b * s_0 + a;

            let coefficients = [vec![a_p], vec![b_p], vec![c_p], vec![d_p]];
            let control_points = CubicBezier::get_control_points(&coefficients);
            let cubic_bezier = CubicBezier::new(control_points);
            let p_vals = cubic_bezier.approximate_linear(eps);
            s_vals.push(s_start);
            for p in p_vals {
                s_vals.push(p.0 * (s_end - s_start) + s_start);
            }
        }

        if s_end - s_vals.last().unwrap() < 1.0e-9 && s_vals.len() != 1 {
            let s_last = s_vals.last_mut().unwrap();
            *s_last = s_end;
        } else {
            s_vals.push(s_end);
        }
        s_vals
    }

    pub fn get(&self, s: f64) -> f64 {
        self.a + self.b * s + self.c * s * s + self.d * s * s * s
    }
}

#[derive(Debug, Clone, Default)]
pub struct CubicSpline {
    s0_to_poly: BTreeMap<OrderedFloat<f64>, Poly3>,
}

impl CubicSpline {
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
            .copied()
            .collect::<BTreeSet<OrderedFloat<f64>>>();
        let mut other_s0 = other
            .s0_to_poly
            .keys()
            .copied()
            .collect::<BTreeSet<OrderedFloat<f64>>>();
        s0_vals.append(&mut other_s0);

        let mut retval = Self::default();

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
                let poly3 = Poly3 {
                    a: this_poly.a + other_poly.a,
                    b: this_poly.b + other_poly.b,
                    c: this_poly.c + other_poly.c,
                    d: this_poly.d + other_poly.d,
                };
                retval.s0_to_poly.insert(s0, poly3);
            }
        }
        retval
    }

    pub fn append_poly3(&mut self, s0: f64, poly3: Poly3) {
        self.s0_to_poly.insert(OrderedFloat(s0), poly3);
    }

    pub fn negate(&self) -> CubicSpline {
        let mut ret = self.clone();

        for (_, poly) in ret.s0_to_poly.iter_mut() {
            poly.negate();
        }

        ret
    }

    pub fn get_poly(&self, s: f64, extend_start: bool) -> Poly3 {
        let nan_poly = Poly3::new(f64::NAN, f64::NAN, f64::NAN, f64::NAN, f64::NAN);
        if self.s0_to_poly.is_empty() {
            return nan_poly;
        }

        let first_poly = self.s0_to_poly.first_key_value().unwrap();

        if s < first_poly.0 .0 {
            if extend_start {
                return *first_poly.1;
            } else {
                return nan_poly;
            }
        }

        let last_poly = self.s0_to_poly.last_key_value().unwrap();

        if s >= last_poly.0 .0 {
            return *last_poly.1;
        }

        let mut s0_poly_iter = self.s0_to_poly.iter().peekable();
        while let Some((s0, poly)) = s0_poly_iter.next() {
            let next_s0 = s0_poly_iter
                .peek()
                .unwrap_or(&(&OrderedFloat(s0.0), poly))
                .0
                 .0;

            if s0.0 <= s && s < next_s0 {
                return *poly;
            }
        }
        // invalid poly result found!
        Poly3 {
            a: f64::NAN,
            b: f64::NAN,
            c: f64::NAN,
            d: f64::NAN,
        }
    }

    pub fn approximate_linear(&self, eps: f64, s_start: f64, s_end: f64) -> Vec<f64> {
        if self.s0_to_poly.is_empty() || s_start == s_end {
            return vec![];
        }

        let mut s_vals = vec![];
        let s_poly_range = self
            .s0_to_poly
            .range(OrderedFloat(s_start)..OrderedFloat(s_end));

        let mut s_poly_iter = s_poly_range.into_iter().peekable();

        while let Some(s_poly) = s_poly_iter.next() {
            let s_start_poly = s_poly.0 .0.max(s_start);
            let s_end_poly = if s_poly_iter.peek().is_none() {
                s_end
            } else {
                s_poly_iter.peek().unwrap().0 .0.min(s_end)
            };
            let mut s_poly_vals = s_poly.1.approximate_linear(eps, s_start_poly, s_end_poly);
            if s_poly_vals.len() < 2 {
                panic!("invalid poly approximation!");
            }
            s_vals.append(&mut s_poly_vals);
        }
        s_vals
    }

    pub fn get(&self, s: f64, default_val: f64, extend_start: bool) -> f64 {
        let poly = self.get_poly(s, extend_start);
        if poly.is_nan() {
            return default_val;
        }
        poly.get(s)
    }
}
