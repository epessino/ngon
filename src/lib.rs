//! # ngon
//!
//! **Introduction**
//! -----
//!
//! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
//! Blah blah
//!

pub mod alias;
pub mod draw;
pub mod tris;

use {
    alias::{Dec2, Vec2},
    cgmath::{prelude::*, vec2},
};

/// Generate an n-gon of `sides` sides, side length scaled down by `side_scale`, transformed by
/// the decomposed translate/rotate/scale `trs` - the vertices of the polygon are generated by
/// rotating the unit vector `from` (unit -y if `None`) around the unit circle at the origin and
/// transforming the result by trs. The output of the function is a set of `sides` lines.
pub fn n_gon(sides: usize, side_scale: f32, trs: &Dec2, from: Option<Vec2>) -> Vec<[Vec2; 2]> {
    let mut lines = vec![[vec2(0.0, 0.0); 2]; sides];

    let angle = std::f32::consts::PI * 2.0 / sides as f32;
    let ss = side_scale.max(0.001).min(1.0);

    let nn = if let Some(f) = from {
        f
    } else {
        vec2(0.0, -1.0)
    }; // starting vector
    let mut v0 = trs.disp + trs.transform_vector(nn);

    for i in 0..sides {
        let an = angle * (i + 1) as f32;
        let (sn, cs) = an.sin_cos();

        let v1 = vec2(nn.x * cs + nn.y * sn, nn.y * cs - nn.x * sn);
        let v1 = trs.disp + trs.transform_vector(v1);

        if ss < 1.0 {
            let cn = v0 + (v1 - v0) * 0.5;
            lines[i] = [cn + (v0 - cn) * ss, cn + (v1 - cn) * ss];
        } else {
            lines[i] = [v0, v1];
        }
        v0 = v1;
    }

    lines
}