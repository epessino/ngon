//! # tris - container of triangles with per vertex colors

use {
    crate::alias::{Color, Vec2},
    cgmath::InnerSpace,
    std::vec::Vec,
};

#[derive(Clone, Debug, Default)]
pub struct Tris {
    pub tr: Vec<[f32; 2]>,
    pub cl: Vec<[f32; 4]>,
}
impl Tris {
    /// Creation
    pub fn new() -> Self {
        Default::default()
    }
    /// Empty the vectors without losing the allocations
    pub fn clear(&mut self) {
        self.tr.clear();
        self.cl.clear();
    }

    /// Add a triangle with a color per vertex
    pub fn add_tri(&mut self, tr: &[Vec2; 3], cl: Color) {
        (0..3).for_each(|i| self.tr.push(tr[i].into()));
        (0..3).for_each(|_| self.cl.push(cl.into()));
    }
    /// Add a triangle with a color per vertex
    pub fn add_tri_cl(&mut self, tr: &[Vec2; 3], cl: &[Color; 3]) {
        (0..3).for_each(|i| self.tr.push(tr[i].into()));
        (0..3).for_each(|i| self.cl.push(cl[i].into()));
    }

    /// Add a quad with a color at each end, optionally make it "filled"
    pub fn add_quad(&mut self, qd: &[Vec2; 4], cl: &[Color; 2]) {
        self.add_quad_cl(qd, &[cl[0], cl[0], cl[1], cl[1]]);
    }
    /// Add a quad with a color per vertex
    pub fn add_quad_cl(&mut self, qd: &[Vec2; 4], cl: &[Color; 4]) {
        self.add_tri_cl(&[qd[0], qd[1], qd[2]], &[cl[0], cl[1], cl[2]]);
        self.add_tri_cl(&[qd[2], qd[3], qd[0]], &[cl[2], cl[3], cl[0]]);
    }
    /// Add a quad with a color at each end, then add a tri to a "center"
    /// location, which is presumably the global center of a set of quads,
    /// so that the result is a loop of quads filled to their global center
    pub fn add_quad_with_filler(&mut self, qd: &[Vec2; 4], cl: &[Color; 2], fill: (Vec2, Color)) {
        self.add_quad_cl(qd, &[cl[0], cl[0], cl[1], cl[1]]);

        // draw a tri to cached/global center
        let (v0, v1) = Self::towards_center(fill.0, &qd);
        self.add_tri_cl(&[fill.0, v0, v1], &[fill.1, cl[0], cl[1]]);
    }

    /// Get the points along cn..(q0/q1) and cn..(q2/q3) needed to make a center tri
    fn towards_center(cn: Vec2, q: &[Vec2; 4]) -> (Vec2, Vec2) {
        (
            if (q[0] - cn).magnitude2() < (q[1] - cn).magnitude2() {
                q[0]
            } else {
                q[1]
            },
            if (q[3] - cn).magnitude2() < (q[2] - cn).magnitude2() {
                q[3]
            } else {
                q[2]
            },
        )
    }
}
