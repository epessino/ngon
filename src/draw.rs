//! # draw - generate rendering data for a variety of shapes

use {
    crate::{
        alias::{Color, Dec2, Mat2, Vec2, Vec4},
        n_gon,
        tris::Tris,
    },
    cgmath::{prelude::*, vec2, vec4},
    num_complex::Complex32,
};

pub struct Draw;
impl Draw {
    /// Add a single line `ps` to the rendering set `tris`
    pub fn line(ps: [Vec2; 2], parms: &DrawParms, tris: &mut Tris) {
        Self::add_line(ps, parms, None, tris);
    }
    /// Draw a set of lines
    pub fn lines(lines: &[[Vec2; 2]], parms: &[DrawParms], tris: &mut Tris) {
        Self::add_lines(lines, parms, None, 1.0, tris);
    }
    /// Draw the set of lines, also fill out to the center (works best if closed, of course)
    pub fn lines_filled(lines: &[[Vec2; 2]], parms: &[DrawParms], tris: &mut Tris) {
        let (center, color) = fill_parms(lines, parms);
        Self::add_lines(lines, parms, Some(Filled { center, color }), 1.0, tris);
    }
    /// Draw the a set of lines generating intermediate parameters from `parm` automatically,
    /// distributing the given parameter space over the estimated linear length of the set.
    ///
    /// If `closed` is true the endpoints are connected. The `scale` parameter can be used to
    /// scale the internal line segments, in conjunction with `parm.connect_subs` this adjusts
    /// the overall curvature of the connections.
    ///
    /// The `scale` parameter is interpreted as follows: 0.0 = automatic absolute (uses
    /// current thickness), (0.0, 1.0] = proportional scale factor around segment centerpoint,
    /// and >1.0 = absolute offset by the given amount, at each end.
    ///
    /// The `lerp_exp` value is used to map the linear blend factor generated for color and
    /// thickness blending from linear to exponential - linear exp 1.0 is used if `None`,
    /// otherwise the value is used as the exponent to raise the blending factor to.
    pub fn lines_auto(
        lines: &[[Vec2; 2]],
        parm: DrawParms,
        scale: f32,
        closed: bool,
        filled: bool,
        lerp_exp: Option<f32>,
        tris: &mut Tris,
    ) {
        let n = lines.len();
        if n == 0 {
            return;
        }

        let lerp_exp = lerp_exp.unwrap_or(1.0);

        let (ls, ln) = corrected_lines(&lines, scale, &parm, closed, lerp_exp);
        let ps = auto_parms(&ls, ln, &parm, closed, lerp_exp);

        if filled {
            let (center, color) = fill_parms(&ls, &ps);
            Self::add_lines(&ls, &ps, Some(Filled { center, color }), lerp_exp, tris);
        } else {
            Self::add_lines(&ls, &ps, None, lerp_exp, tris);
        }
    }

    /// Draw a circle (generates a 4-gon and uses exp sweeps proportional to radius)
    pub fn circle(center: Vec2, radius: f32, color: Color, thickness: f32, tris: &mut Tris) {
        let dp = DrawParms::new()
            .color([color; 2])
            .thickness([thickness; 2])
            .connect_subs(f32::clamp(radius / 3.0, 1.0, 16.0) as u16);
        let trs = Dec2 {
            scale: radius * f32::sqrt(2.0), // compensate for the side_scale we need to use the connectors
            disp: center,
            ..Dec2::one()
        };
        Self::lines(&n_gon(4, 0.001, &trs, None), &[dp; 4], tris);
    }
    /// Draw a filled circle (draws two rounded caps)
    pub fn disk(center: Vec2, radius: f32, color: Color, subs: u16, tris: &mut Tris) {
        let pm = DrawParms {
            color: [color, color],
            thickness: [radius, radius],
            cap: [CapStyle::None, CapStyle::None],
            connect_subs: 0,
        };

        let norm = vec2(0.0, 1.0);
        let subs = u16::max(2, subs / 2);

        round_t(center, norm, subs, 0, &pm, tris);
        round_t(center, norm, subs, 1, &pm, tris);
    }
    /// Draw an axis-aligned rectangle
    pub fn rect(
        &mut self,
        bounds: [Vec2; 2],
        color: Color,
        thickness: f32,
        filled: bool,
        tris: &mut Tris,
    ) {
        let ht = thickness / 2.0;
        let pm = DrawParms {
            color: [color, color],
            thickness: [ht, ht],
            cap: [CapStyle::Square(0.0), CapStyle::None],
            connect_subs: 0,
        };

        let tl = bounds[0];
        let br = bounds[1];
        let tr = vec2(br.x, tl.y);
        let bl = vec2(tl.x, br.y);

        let ln = [
            [tl, vec2(br.x - ht, tl.y)],
            [tr, vec2(br.x, br.y - ht)],
            [br, vec2(tl.x + ht, br.y)],
            [bl, vec2(tl.x, tl.y + ht)],
        ];
        if filled {
            Self::lines_filled(&ln, &[pm; 4], tris);
        } else {
            Self::lines(&ln, &[pm; 4], tris);
        }
    }
    /// Draw a filled quad with a color per vertex, no other options
    pub fn quad(quad: &[Vec2; 4], cols: &[Color; 4], tris: &mut Tris) {
        tris.add_quad_cl(quad, cols);
    }

    /// Draw a filled `aabb` quad (`bounds` is `[tl, br]`), single color
    pub fn aabb(&mut self, bounds: &[Vec2; 2], col: Color, tris: &mut Tris) {
        let tl = bounds[0];
        let br = bounds[1];
        let tr = vec2(br.x, tl.y);
        let bl = vec2(tl.x, br.y);
        tris.add_quad_cl(&[tl, tr, br, bl], &[col; 4]);
    }

    /// Add a single line `ps` to the rendering set `tris`
    fn add_line(ps: [Vec2; 2], parms: &DrawParms, filled: Option<Filled>, tris: &mut Tris) {
        let (ln, nn) = line_parms(ps);

        let p0 = match parms.cap[0] {
            CapStyle::Square(n) => ps[0] - ln * if n > 0.0 { n } else { parms.thickness[0] },
            _ => ps[0],
        };
        let p1 = match parms.cap[1] {
            CapStyle::Square(n) => ps[1] + ln * if n > 0.0 { n } else { parms.thickness[1] },
            _ => ps[1],
        };

        let n0 = nn * parms.thickness[0];
        let n1 = nn * parms.thickness[1];

        Self::add_quad(
            &[p0 - n0, p0 + n0, p1 + n1, p1 - n1],
            &parms.color,
            filled,
            tris,
        );

        match parms.cap[0] {
            CapStyle::Round(sub) => round_t(ps[0], nn, sub, 0, parms, tris),
            _ => (),
        };
        match parms.cap[1] {
            CapStyle::Round(sub) => round_t(ps[1], nn, sub, 1, parms, tris),
            _ => (),
        };
    }
    /// Draw a set of lines
    fn add_lines(
        lines: &[[Vec2; 2]],
        parms: &[DrawParms],
        filled: Option<Filled>,
        lerp_exp: f32,
        tris: &mut Tris,
    ) {
        let n = lines.len();
        assert_eq!(n, parms.len());

        (0..n).for_each(|i| {
            Self::add_line(lines[i], &parms[i], filled, tris);
            if parms[i].connect_subs > 0 {
                let j = if i < n - 1 { i + 1 } else { 0 };

                let (_, nn0) = line_parms(lines[i]);
                let (_, nn1) = line_parms(lines[j]);

                let n0 = nn0 * parms[i].thickness[1];
                let f = [lines[i][1] + n0, lines[i][1] - n0];

                let n1 = nn1 * parms[j].thickness[0];
                let t = [lines[j][0] + n1, lines[j][0] - n1];

                let cols = [parms[i].color[1], parms[j].color[0]];
                connect(f, t, parms[i].connect_subs, cols, lerp_exp, tris);
            }
        });
    }
    /// Add a quad, if in filled mode also generate tri to center
    fn add_quad(quad: &[Vec2; 4], cols: &[Color; 2], filled: Option<Filled>, tris: &mut Tris) {
        if let Some(Filled { center, color }) = filled {
            tris.add_quad_with_filler(quad, cols, (center, color));
        } else {
            tris.add_quad(quad, cols);
        }
    }
}

/// Line segment rendering parameters
#[derive(Copy, Clone, Debug)]
pub struct DrawParms {
    /// Colors at time 0.0 and 1.0
    pub color: [Color; 2],
    /// Thickness (on each side) at time 0.0 and 1.0
    pub thickness: [f32; 2],
    /// End cap style at 0.0 and 1.0
    pub cap: [CapStyle; 2],
    /// Subdivisions to connect to next line in a set (0 to disable)
    pub connect_subs: u16,
}
impl DrawParms {
    /// Build a draw parameter record
    pub fn new() -> Self {
        DrawParms {
            color: [vec4(1.0, 1.0, 1.0, 1.0), vec4(1.0, 1.0, 1.0, 1.0)],
            thickness: [1.0, 1.0],
            cap: [CapStyle::None, CapStyle::None],
            connect_subs: 16,
        }
    }

    /// Builder: set the color
    pub fn color(mut self, c: [Color; 2]) -> Self {
        self.color = c;
        self
    }
    /// Builder: set the thickness
    pub fn thickness(mut self, t: [f32; 2]) -> Self {
        self.thickness = t;
        self
    }
    /// Builder: set the endpoint style
    pub fn cap(mut self, c: [CapStyle; 2]) -> Self {
        self.cap = c;
        self
    }
    /// Builder: set number of subdivisions for the connector (0 = no connector)
    pub fn connect_subs(mut self, n: u16) -> Self {
        self.connect_subs = n;
        self
    }
}

/// Drawing styles available for each end cap
#[derive(Copy, Clone, Debug)]
pub enum CapStyle {
    /// Rounded end cap with given subdivisions
    Round(u16),
    /// Squared end cap, to given distance (0.0 == use thickness)
    Square(f32),
    /// No cap
    None,
}

/// Record used to describe how to generate "fill" triangles
#[derive(Copy, Clone, Debug)]
pub struct Filled {
    /// Center point of the region to be filled
    pub center: Vec2,
    /// Color at center of fill region, if specified
    pub color: Color,
}
impl Default for Filled {
    fn default() -> Self {
        Self {
            center: vec2(0.0, 0.0),
            color: vec4(0.0, 0.0, 0.0, 0.0),
        }
    }
}

/// Get direction and normal vectors for the given line
fn line_parms(line: [Vec2; 2]) -> (Vec2, Vec2) {
    let ln = vec2_norm_or_zero(line[1] - line[0]);
    let nn = vec2(-ln.y, ln.x);

    (ln, nn)
}
/// Maximum precision normalization or 0-vector
fn vec2_norm_or_zero(v: Vec2) -> Vec2 {
    let sz = 1.0 / v[0].hypot(v[1]);
    if sz.is_finite() {
        v * sz
    } else {
        vec2(0.0, 0.0)
    }
}

/// Draw a rounded cap at `at`
pub fn round_t(
    at: Vec2,
    normal: Vec2, // normal at endpoint
    subs: u16,    // desired subdivisions
    index: usize, // 0/1 index to get extra endpoint data
    parms: &DrawParms,
    tris: &mut Tris,
) {
    round_t_fn(
        at,
        normal,
        subs,
        index,
        parms,
        &mut |p: &[Vec2; 3], c: &[Color; 3]| tris.add_tri_cl(p, c),
    );
}
/// Call a closure for each generated triangle of rounded cap at `at`
pub fn round_t_fn<F: FnMut(&[Vec2; 3], &[Color; 3])>(
    at: Vec2,
    normal: Vec2, // normal at endpoint
    subs: u16,    // desired subdivisions
    index: usize, // 0/1 index to get extra endpoint data
    parms: &DrawParms,
    f: &mut F,
) {
    if subs == 0 {
        return;
    }

    let p0 = at; // base
    let p1 = at + normal * parms.thickness[index]; // moving points

    let mut p = [p0, p1, vec2(0.0, 0.0)];
    let cols = [parms.color[index]; 3];

    let tris = subs as usize; // number of generated triangles
    let angle = if index == 0 {
        std::f32::consts::PI / tris as f32
    } else {
        -std::f32::consts::PI / tris as f32
    };

    for i in 0..tris {
        let an = angle * (i + 1) as f32;
        let (sn, cs) = an.sin_cos();

        let n2 = vec2(normal.x * cs - normal.y * sn, normal.y * cs + normal.x * sn);
        p[2] = at + n2 * parms.thickness[index];

        f(&p, &cols);
        p[1] = p[2];
    }
}

/// Generate quads connecting a segment to another - uses the Hise Vector Screw
fn connect_t(
    f: [Vec2; 2],    // from segment
    t: [Vec2; 2],    // to segment
    about: Vec2,     // initial pivot point
    subs: u16,       // number of quads to generate
    col: [Color; 2], // colors at f and t
    lerp_exp: f32,
    tris: &mut Tris,
) {
    let cn0 = corrected_pivot([f[0], t[0]], about);
    let cn1 = corrected_pivot([f[1], t[1]], about);

    let f0c = complex(f[0]) - cn0;
    let t0c = complex(t[0]) - cn0;
    let f1c = complex(f[1]) - cn1;
    let t1c = complex(t[1]) - cn1;

    let d0 = f0c.inv() * t0c;
    let d1 = f1c.inv() * t1c;

    let cf = as_vec4(col[0]); // colors for interpolation
    let ct = as_vec4(col[1]);

    let mut q0 = complex(f[0]);
    let mut q1 = complex(f[1]);

    let mut c0 = as_color(cf);

    for i in 0..subs {
        let ex = ((i + 1) as f32 / subs as f32).powf(lerp_exp); // exponent to interpolate over
        let c1 = col_lerp(ex, cf, ct);

        let p0 = if d0.im >= 0.0 {
            d0.powf(ex)
        } else {
            d0.conj().powf(ex).conj() // map to positive and back
        };
        let p1 = if d1.im >= 0.0 {
            d1.powf(ex)
        } else {
            d1.conj().powf(ex).conj()
        };

        let p0 = f0c * p0 + cn0;
        let p1 = f1c * p1 + cn1;

        tris.add_quad(
            &[
                vec2(q0.re, q0.im),
                vec2(q1.re, q1.im),
                vec2(p1.re, p1.im),
                vec2(p0.re, p0.im),
            ],
            &[c0, c1],
        );

        q0 = p0;
        q1 = p1;
        c0 = c1;
    }
}

/// Connect segment f to segment t with an arc
fn connect(f: [Vec2; 2], t: [Vec2; 2], subs: u16, col: [Color; 2], lerp_exp: f32, tris: &mut Tris) {
    assert!(subs > 0);

    let df = f[1] - f[0];
    let dt = t[1] - t[0];

    // Find the intersection of the line segments as the initial pivot point
    let mat = Mat2::new(df.x, df.y, dt.x, dt.y);
    let det = mat.determinant();

    if det.abs() < 0.001 {
        tris.add_quad(&[f[0], f[1], t[1], t[0]], &col);
    } else if let Some(mat) = mat.invert() {
        connect_t(
            f,
            t,
            f[0] + (f[1] - f[0]) * (mat * (t[0] - f[0])).x,
            subs,
            col,
            lerp_exp,
            tris,
        );
    }
}

/// Check and correct the pivot point for the given points
fn corrected_pivot(points: [Vec2; 2], about: Vec2) -> Complex32 {
    let p0c = complex(points[0] - about);
    let p1c = complex(points[1] - about);

    complex(if p0c.norm() < 0.01 {
        -points[1]
    } else if p1c.norm() < 0.01 {
        -points[0]
    } else {
        about
    })
}

/// Return `v` as a complex number
pub fn complex(v: Vec2) -> Complex32 {
    Complex32::new(v.x, v.y)
}

/// Scale the endpoints of a line segment about its center by scale[left, right]
fn line_scaled(line: [Vec2; 2], scale: [f32; 2]) -> [Vec2; 2] {
    let cn = line[0] + (line[1] - line[0]) * 0.5;
    [
        cn + (line[0] - cn) * scale[0],
        cn + (line[1] - cn) * scale[1],
    ]
}

/// Compute the corrected version of the lines - returns a vector of corrected lines and the
/// estimated total linear length of the new set
fn corrected_lines(
    lines: &[[Vec2; 2]],
    scale: f32,
    parm: &DrawParms,
    closed: bool,
    lerp_exp: f32,
) -> (Vec<[Vec2; 2]>, f32) {
    let n = lines.len();
    if n == 0 {
        return (vec![], 0.0);
    }
    let connected = parm.connect_subs > 0;

    // Calculate the total estimated linear length
    let mut len = 0.0;
    for i in 0..n {
        len += (lines[i][1] - lines[i][0]).magnitude();
    }
    if closed {
        len += (lines[0][0] - lines[n - 1][1]).magnitude();
    }

    // Now generate corrected lines, recompute the total length as we go
    let mut ls: Vec<[Vec2; 2]> = Vec::with_capacity(n); // converted lines
    let mut ln = 0.0; // new total estimated linear length

    let mut l = 0.0; // current position along the initial estimated length
    let thd = parm.thickness[1] - parm.thickness[0]; // linear thickness delta

    for i in 0..n {
        let ll = (lines[i][1] - lines[i][0]).magnitude(); // uncorrected length

        let t0 = (l / len).powf(lerp_exp); // blend factor at each end of the line
        let t1 = ((l + ll) / len).powf(lerp_exp);

        let th_0 = parm.thickness[0] + thd * t0;
        let th_1 = parm.thickness[0] + thd * t1;

        let cl = corrected_line(lines[i], i, n, scale, [th_0, th_1], closed);

        ln += (cl[1] - cl[0]).magnitude(); // recalculate lengths as we go
        if connected && i > 0 {
            ln += (cl[0] - ls[i - 1][1]).magnitude();
        }

        ls.push(cl);
        l += ll;
    }

    if closed {
        ln += (ls[0][0] - ls[n - 1][1]).magnitude();
    }

    (ls, ln)
}
/// Given a `line`, its index `i`, the total number of lines `n`, the correction factor `scale`
/// and the thickness at line[0] and line[1] `thickness` calculate and return the line
/// corrected as determined by `scale`
fn corrected_line(
    line: [Vec2; 2],
    i: usize,
    n: usize,
    scale: f32,
    thickness: [f32; 2],
    closed: bool,
) -> [Vec2; 2] {
    if n == 1 {
        return line; // one line, no change in any mode
    }

    let mut c = line;

    // If scale in (0.0..1.0] then we apply proportional scaling
    if scale > 0.0 && scale <= 1.0 {
        if i == 0 && closed == false {
            c = line_scaled(line, [1.0, scale]);
        } else if i == n - 1 && closed == false {
            c = line_scaled(line, [scale, 1.0]);
        } else {
            c = line_scaled(line, [scale, scale]);
        }
    } else {
        let v = c[1] - c[0];
        let m = v[0].hypot(v[1]);
        let sz = 1.0 / m;

        if sz.is_finite() {
            let l0;
            let l1;

            let cn = c[0] + v * 0.5; // center of line
            let vn = v * sz; // normalized vector

            if scale > 1.0 {
                // absolute
                l0 = m * 0.5 - f32::min(m * 0.5, scale); // apply at most half a line length
                l1 = l0;
            } else {
                // scale <= 0.0, automatic
                l0 = m * 0.5 - f32::min(m * 0.5, thickness[0] * 1.2);
                l1 = m * 0.5 - f32::min(m * 0.5, thickness[1] * 1.2);
            }

            if i == 0 && closed == false {
                c = [c[0], cn + vn * l1];
            } else if i == n - 1 && closed == false {
                c = [cn - vn * l0, c[1]];
            } else {
                c = [cn - vn * l0, cn + vn * l1];
            }
        }
    }

    c
}

/// Derive a full set of draw parms interpolating over the line segments from `parm`
fn auto_parms(
    lines: &[[Vec2; 2]],
    tot_len: f32,
    parm: &DrawParms,
    closed: bool,
    lerp_exp: f32,
) -> Vec<DrawParms> {
    let n = lines.len();
    if n == 0 {
        return vec![];
    }

    let mut ps = vec![
        DrawParms {
            cap: [CapStyle::None, CapStyle::None],
            ..*parm
        };
        n
    ];

    ps[0].cap[0] = parm.cap[0]; // preserve the end caps at beginning and end
    ps[n - 1].cap[1] = parm.cap[1];

    if !closed {
        ps[n - 1].connect_subs = 0;
    }

    let mut len = 0.0;
    let connected = parm.connect_subs > 0;

    let c0 = as_vec4(parm.color[0]);
    let c1 = as_vec4(parm.color[1]);

    for i in 0..n {
        let t = (len / tot_len).powf(lerp_exp);

        ps[i].color[0] = col_lerp(t, c0, c1);
        ps[i].thickness[0] = parm.thickness[0] + (parm.thickness[1] - parm.thickness[0]) * t;

        len += (lines[i][1] - lines[i][0]).magnitude();

        let t = (len / tot_len).powf(lerp_exp);

        ps[i].color[1] = col_lerp(t, c0, c1);
        ps[i].thickness[1] = parm.thickness[0] + (parm.thickness[1] - parm.thickness[0]) * t;

        if connected && i < n - 1 {
            len += (lines[i + 1][0] - lines[i][1]).magnitude();
        }
    }

    ps
}

/// Calculate the center point of the given lines
fn fill_parms(lines: &[[Vec2; 2]], parms: &[DrawParms]) -> (Vec2, Color) {
    let mut center = vec2(0.0, 0.0);
    let mut color = vec4(0.0, 0.0, 0.0, 0.0);

    let n = lines.len();
    for i in 0..n {
        center += lines[i][0] + lines[i][1];
        color += as_vec4(parms[i].color[0]) + as_vec4(parms[i].color[1]);
    }

    (center / (n * 2) as f32, as_color(color / (n * 2) as f32))
}

/// Return a color as a vec4
#[inline(always)]
pub fn as_vec4(c: Color) -> Vec4 {
    vec4(c[0], c[1], c[2], c[3])
}
/// Return the vec4 as a color
#[inline(always)]
pub fn as_color(v: Vec4) -> Color {
    vec4(v.x, v.y, v.z, v.w)
}

/// Simple lerp between two colors
#[inline(always)]
pub fn col_lerp(t: f32, a: Vec4, b: Vec4) -> Color {
    as_color(a + (b - a) * t)
}
