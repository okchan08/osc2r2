use std::{
    collections::{BTreeMap, BTreeSet},
    ops::Index,
};

use ordered_float::OrderedFloat;

pub struct CubicBezier {
    control_points: [Vec<f64>; 4],
    valid_length: f64,
    arclen_t: BTreeMap<OrderedFloat<f64>, f64>,
}

impl CubicBezier {
    pub fn new(control_points: [Vec<f64>; 4]) -> Self {
        let length_to_tolerance = 1.0e-2;
        let mut cubic_bezier = CubicBezier {
            control_points,
            valid_length: 0.0,
            arclen_t: BTreeMap::new(),
        };
        let t_vals = cubic_bezier.approximate_linear(length_to_tolerance);
        if t_vals.len() < 2 {
            panic!("expected at least two t values");
        }

        cubic_bezier.arclen_t.insert(OrderedFloat(0.0), 0.0);

        let mut arclen = 0.0;

        for t_val in t_vals.iter() {
            if t_val.0 == t_vals.first().unwrap().0 {
                continue;
            }
            let prev_t_val = t_vals.range(..t_val).next_back().unwrap();
            let pt_prev = cubic_bezier.get(prev_t_val.0);
            let pt = cubic_bezier.get(t_val.0);
            let length = pt
                .iter()
                .zip(pt_prev.iter())
                .fold(0.0, |total: f64, (a, b)| (a - b) * (a - b) + total)
                .sqrt();
            arclen += length;
            cubic_bezier.arclen_t.insert(OrderedFloat(arclen), t_val.0);
        }

        cubic_bezier.valid_length = cubic_bezier.arclen_t.last_key_value().unwrap().0 .0;

        cubic_bezier
    }
    pub fn get_control_points(coefficients: &[Vec<f64>; 4]) -> [Vec<f64>; 4] {
        let a = &coefficients[0];

        let dim = a.len();

        let b = &coefficients[1];
        let c = &coefficients[2];
        let d = &coefficients[3];

        assert_eq!(b.len(), dim);
        assert_eq!(c.len(), dim);
        assert_eq!(d.len(), dim);

        let mut ctrl_pts = [
            vec![0.0; dim],
            vec![0.0; dim],
            vec![0.0; dim],
            vec![0.0; dim],
        ];

        ctrl_pts[0] = a.to_vec();

        for (idx, _) in b.iter().enumerate() {
            ctrl_pts[1][idx] = (b[idx] / 3.0) + a[idx];
        }

        for (idx, _) in c.iter().enumerate() {
            ctrl_pts[2][idx] = (c[idx] / 3.0) + 2.0 * ctrl_pts[1][idx] - ctrl_pts[0][idx];
        }

        for (idx, _) in d.iter().enumerate() {
            ctrl_pts[3][idx] =
                d[idx] + 3.0 * ctrl_pts[2][idx] - 3.0 * ctrl_pts[1][idx] + ctrl_pts[0][idx];
        }

        ctrl_pts
    }

    pub fn get_coefficients(control_points: &[Vec<f64>; 4]) -> [Vec<f64>; 4] {
        let p_a = &control_points[0];
        let p_b = &control_points[1];
        let p_c = &control_points[2];
        let p_d = &control_points[3];

        let dim = p_a.len();
        assert_eq!(p_b.len(), dim);
        assert_eq!(p_c.len(), dim);
        assert_eq!(p_d.len(), dim);

        let mut coefficients = [
            vec![0.0; dim],
            vec![0.0; dim],
            vec![0.0; dim],
            vec![0.0; dim],
        ];

        coefficients[0] = p_a.to_vec();

        for idx in 0..dim {
            coefficients[1][idx] = 3.0 * p_b[idx] - 3.0 * p_a[idx];
        }

        for idx in 0..dim {
            coefficients[2][idx] = 3.0 * p_c[idx] - 6.0 * p_b[idx] + 3.0 * p_a[idx];
        }

        for idx in 0..dim {
            coefficients[3][idx] = p_d[idx] - 3.0 * p_c[idx] + 3.0 * p_b[idx] - p_a[idx];
        }

        coefficients
    }

    pub fn approximate_linear(&self, eps: f64) -> BTreeSet<OrderedFloat<f64>> {
        let coefficients = CubicBezier::get_coefficients(&self.control_points);
        let dim = coefficients[0].len();

        let norm = |vec: &Vec<f64>| -> f64 {
            vec.iter()
                .fold(0.0, |total: f64, x: &f64| total + x * x)
                .sqrt()
        };

        let seg_size = (0.5 * eps / ((1.0 / 54.0) * norm(&coefficients[3]))).powf(1.0 / 3.0);

        let mut seg_intervals: Vec<[f64; 2]> = Vec::new();

        {
            let mut t = 0.0;
            while t < 1.0 {
                seg_intervals.push([
                    t,
                    std::cmp::min(OrderedFloat(t + seg_size), OrderedFloat(1.0)).0,
                ]);
                t += seg_size;
            }
        }

        if 1.0 - seg_intervals.last().unwrap().index(1) < 1.0e-6 {
            seg_intervals.last_mut().unwrap()[1] = 1.0;
        } else {
            seg_intervals.push([*seg_intervals.last().unwrap().index(1), 1.0]);
        }

        let mut t_vals = vec![0.0];
        for seg_intrvl in seg_intervals.iter() {
            let (t0, t1) = (seg_intrvl[0], seg_intrvl[1]);

            let c_pts_sub = self.get_subcurve(t0, t1);

            /* approximate sub-cubic bezier by two quadratic ones */
            let mut p_b_quad_0 = vec![0.0; dim];
            for (idx, item) in p_b_quad_0.iter_mut().enumerate() {
                *item = (1.0 - 0.75) * c_pts_sub[0][idx] + 0.75 * c_pts_sub[1][idx];
            }
            let mut p_b_quad_1 = vec![0.0; dim];
            for (idx, item) in p_b_quad_1.iter_mut().enumerate() {
                *item = (1.0 - 0.75) * c_pts_sub[3][idx] + 0.75 * c_pts_sub[2][idx];
            }
            let mut p_m_quad = vec![0.0; dim];
            for (idx, item) in p_m_quad.iter_mut().enumerate() {
                *item = (1.0 - 0.5) * p_b_quad_0[idx] + 0.5 * p_b_quad_1[idx];
            }

            for p_sub in approximate_linear_quad_bezier(
                &[c_pts_sub[0].to_vec(), p_b_quad_0, p_m_quad.to_vec()],
                eps * 0.5,
            ) {
                t_vals.push(t0 + p_sub * (t1 - t0) * 0.5);
            }
            t_vals.pop();

            for p_sub in approximate_linear_quad_bezier(
                &[p_m_quad.to_vec(), p_b_quad_1, c_pts_sub[3].to_vec()],
                eps * 0.5,
            ) {
                t_vals.push(t0 + (t1 - t0) * 0.5 + p_sub * (t1 - t0) * 0.5);
            }
            t_vals.pop();
        }
        t_vals.push(1.0);

        t_vals.into_iter().map(OrderedFloat).collect()
    }

    fn get_subcurve(&self, t_start: f64, t_end: f64) -> [Vec<f64>; 4] {
        let f_cubic_t123 = |t1: f64, t2: f64, t3: f64, ctrl_pts: &[Vec<f64>; 4]| -> Vec<f64> {
            let dim = ctrl_pts[0].len();
            let mut out = vec![0.0; dim];
            for (idx, item) in out.iter_mut().enumerate() {
                *item = (1.0 - t3)
                    * ((1.0 - t2) * ((1.0 - t1) * ctrl_pts[0][idx] + t1 * ctrl_pts[1][idx])
                        + t2 * ((1.0 - t1) * ctrl_pts[1][idx] + t1 * ctrl_pts[2][idx]))
                    + t3 * ((1.0 - t2) * ((1.0 - t1) * ctrl_pts[1][idx] + t1 * ctrl_pts[2][idx])
                        + t2 * ((1.0 - t1) * ctrl_pts[2][idx] + t1 * ctrl_pts[3][idx]));
            }
            out
        };

        [
            f_cubic_t123(t_start, t_start, t_start, &self.control_points),
            f_cubic_t123(t_start, t_start, t_end, &self.control_points),
            f_cubic_t123(t_start, t_end, t_end, &self.control_points),
            f_cubic_t123(t_end, t_end, t_end, &self.control_points),
        ]
    }

    fn get(&self, t: f64) -> Vec<f64> {
        let dim = self.control_points[0].len();
        let mut out_pt = vec![0.0; dim];
        for (idx, item) in out_pt.iter_mut().enumerate() {
            *item = (1.0 - t) * (1.0 - t) * (1.0 - t) * self.control_points[0][idx]
                + 3.0 * t * (1.0 - t) * (1.0 - t) * self.control_points[1][idx]
                + 3.0 * t * t * (1.0 - t) * self.control_points[2][idx]
                + t * t * t * self.control_points[3][idx];
        }
        out_pt
    }
}

fn approximate_linear_quad_bezier(ctrl_pts: &[Vec<f64>; 3], eps: f64) -> Vec<f64> {
    let dim = ctrl_pts[0].len();
    let mut param_c = vec![0.0; dim];

    for (idx, item) in param_c.iter_mut().enumerate() {
        *item = ctrl_pts[0][idx] - 2.0 * ctrl_pts[1][idx] + ctrl_pts[2][idx];
    }

    let norm = |vec: &Vec<f64>| -> f64 {
        vec.iter()
            .fold(0.0, |total: f64, x: &f64| total + x * x)
            .sqrt()
    };
    let step_size = std::cmp::min(
        OrderedFloat(((4.0 * eps) / norm(&param_c)).sqrt()),
        OrderedFloat(1.0),
    )
    .0;

    let mut p_vals = vec![];
    {
        let mut p = 0.0;
        while p < 1.0 {
            p_vals.push(p);
            p += step_size;
        }
    }

    if *p_vals.last().unwrap() != 1.0 {
        p_vals.push(1.0);
    }
    p_vals
}
