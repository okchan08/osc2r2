use super::road_geometry::{GeometryType, RoadGeometry};
use crate::open_drive::math::Vec2;
use std::f64::consts::PI;

#[derive(Debug, Clone, Copy)]
pub struct Spiral {
    s0: f64,
    x0: f64,
    y0: f64,
    hdg0: f64,
    length: f64,

    curv_start: f64,
    curv_end: f64,
    s_start: f64,
    s_end: f64,
    c_dot: f64,

    s0_spiral: f64,
    x0_spiral: f64,
    y0_spiral: f64,
    a0_spiral: f64,
}

impl Spiral {
    pub fn new(
        s0: f64,
        x0: f64,
        y0: f64,
        hdg0: f64,
        length: f64,
        curv_start: f64,
        curv_end: f64,
    ) -> Self {
        let mut spiral = Spiral {
            s0: s0,
            x0: x0,
            y0: y0,
            hdg0: hdg0,
            length: length,
            curv_start: curv_start,
            curv_end: curv_end,
            s_start: 0.0,
            s_end: 0.0,
            c_dot: 0.0,
            s0_spiral: 0.0,
            x0_spiral: 0.0,
            y0_spiral: 0.0,
            a0_spiral: 0.0,
        };
        let c_dot = (spiral.curv_end - spiral.curv_start) / spiral.length;
        let s0_spiral = spiral.curv_start / c_dot;
        let (x0_spiral, y0_spiral, a0_spiral) = odr_spiral(s0_spiral, c_dot);
        spiral.s_start = spiral.curv_start / c_dot;
        spiral.s_end = spiral.curv_end / c_dot;
        spiral.c_dot = c_dot;
        spiral.s0_spiral = s0_spiral;
        spiral.x0_spiral = x0_spiral;
        spiral.y0_spiral = y0_spiral;
        spiral.a0_spiral = a0_spiral;
        spiral
    }
}

impl RoadGeometry for Spiral {
    fn get_xy(&self, s: f64) -> Vec2 {
        let (xs_spiral, ys_spiral, _) = odr_spiral(s - self.s0 + self.s0_spiral, self.c_dot);
        let hdg = self.hdg0 - self.a0_spiral;
        let xt = (hdg.cos() * (xs_spiral - self.x0_spiral))
            - (hdg.sin() * (ys_spiral - self.y0_spiral))
            + self.x0;
        let yt = (hdg.sin() * (xs_spiral - self.x0_spiral))
            - (hdg.cos() * (ys_spiral - self.y0_spiral))
            + self.y0;
        Vec2(xt, yt)
    }

    fn get_grad(&self, s: f64) -> Vec2 {
        let (_, _, as_spiral) = odr_spiral(s - self.s0 + self.s0_spiral, self.c_dot);
        let hdg = as_spiral + self.hdg0 - self.a0_spiral;
        Vec2(hdg.cos(), hdg.sin())
    }

    fn approximate_linear(&self, eps: f64) -> Vec<f64> {
        // TODO: properly implement
        let mut retval = vec![];
        let mut curr_s = self.s0;
        while curr_s < (self.s0 + self.length) {
            retval.push(curr_s);
            curr_s += 10.0 * eps;
        }
        retval
    }

    fn geometry_type(&self) -> GeometryType {
        GeometryType::GeometryTypeSpiral
    }
}

fn polevl(x: f64, coef: &[f64], n: usize) -> f64 {
    let mut ans;
    let mut i;

    ans = coef[0];
    i = n;

    while i > 0 {
        i -= 1;
        ans = ans * x + coef[i];
    }
    return ans;
}

fn p1evl(x: f64, coef: &[f64], n: usize) -> f64 {
    let mut ans;
    let mut i;

    ans = x + coef[0];
    i = n - 1;

    while i > 0 {
        i -= 1;
        ans = ans * x + coef[i];
    }
    ans
}

/* S(x) for small x */
const SN: [f64; 6] = [
    -2.99181919401019853726E3,
    7.08840045257738576863E5,
    -6.29741486205862506537E7,
    2.54890880573376359104E9,
    -4.42979518059697779103E10,
    3.18016297876567817986E11,
];
const SD: [f64; 6] = [
    /* 1.00000000000000000000E0,*/
    2.81376268889994315696E2,
    4.55847810806532581675E4,
    5.17343888770096400730E6,
    4.19320245898111231129E8,
    2.24411795645340920940E10,
    6.07366389490084639049E11,
];

/* C(x) for small x */
const CN: [f64; 6] = [
    -4.98843114573573548651E-8,
    9.50428062829859605134E-6,
    -6.45191435683965050962E-4,
    1.88843319396703850064E-2,
    -2.05525900955013891793E-1,
    9.99999999999999998822E-1,
];
const CD: [f64; 7] = [
    3.99982968972495980367E-12,
    9.15439215774657478799E-10,
    1.25001862479598821474E-7,
    1.22262789024179030997E-5,
    8.68029542941784300606E-4,
    4.12142090722199792936E-2,
    1.00000000000000000118E0,
];

/* Auxiliary function f(x) */
const FNS: [f64; 10] = [
    4.21543555043677546506E-1,
    1.43407919780758885261E-1,
    1.15220955073585758835E-2,
    3.45017939782574027900E-4,
    4.63613749287867322088E-6,
    3.05568983790257605827E-8,
    1.02304514164907233465E-10,
    1.72010743268161828879E-13,
    1.34283276233062758925E-16,
    3.76329711269987889006E-20,
];
const FD: [f64; 10] = [
    /*  1.00000000000000000000E0,*/
    7.51586398353378947175E-1,
    1.16888925859191382142E-1,
    6.44051526508858611005E-3,
    1.55934409164153020873E-4,
    1.84627567348930545870E-6,
    1.12699224763999035261E-8,
    3.60140029589371370404E-11,
    5.88754533621578410010E-14,
    4.52001434074129701496E-17,
    1.25443237090011264384E-20,
];

/* Auxiliary function g(x) */
const GN: [f64; 11] = [
    5.04442073643383265887E-1,
    1.97102833525523411709E-1,
    1.87648584092575249293E-2,
    6.84079380915393090172E-4,
    1.15138826111884280931E-5,
    9.82852443688422223854E-8,
    4.45344415861750144738E-10,
    1.08268041139020870318E-12,
    1.37555460633261799868E-15,
    8.36354435630677421531E-19,
    1.86958710162783235106E-22,
];
const GD: [f64; 11] = [
    /*  1.00000000000000000000E0,*/
    1.47495759925128324529E0,
    3.37748989120019970451E-1,
    2.53603741420338795122E-2,
    8.14679107184306179049E-4,
    1.27545075667729118702E-5,
    1.04314589657571990585E-7,
    4.60680728146520428211E-10,
    1.10273215066240270757E-12,
    1.38796531259578871258E-15,
    8.39158816283118707363E-19,
    1.86958710162783236342E-22,
];

fn fresnel(xxa: f64) -> (f64, f64) {
    let (f, g, mut cc, mut ss, c, s, mut t, u);
    let (x, mut x2);

    x = xxa.abs();
    x2 = x * x;

    if x2 < 2.5625 {
        t = x2 * x2;
        ss = x * x2 * polevl(t, &SN, 5) / p1evl(t, &SD, 6);
        cc = x * polevl(t, &CN, 5) / polevl(t, &CD, 6);
    } else if x > 36974.0 {
        cc = 0.5;
        ss = 0.5;
    } else {
        x2 = x * x;
        t = PI * x2;
        u = 1.0 / (t * t);
        t = 1.0 / t;
        f = 1.0 - u * polevl(u, &FNS, 9) / p1evl(u, &FD, 10);
        g = t * polevl(u, &GN, 10) / p1evl(u, &GD, 11);

        t = PI * 0.5 * x2;
        c = t.cos();
        s = t.sin();
        t = PI * x;
        cc = 0.5 + (f * s - g * c) / t;
        ss = 0.5 - (f * c + g * s) / t;
    }

    if xxa < 0.0 {
        cc = -cc;
        ss = -ss;
    }
    (cc, ss)
}

// TODO: fix bug in the estimation.
fn odr_spiral(s: f64, c_dot: f64) -> (f64, f64, f64) {
    let mut a = 1.0 / c_dot.abs().sqrt();
    a *= PI.sqrt();

    let (mut y, mut x) = fresnel(s / a);

    x = x * a;
    y = y * a;

    if c_dot < 0.0 {
        y = -1.0 * y;
    }

    (x, y, s * s * c_dot * 0.5)
}
