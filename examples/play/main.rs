//!
//! # Creates a window in piston and converts into resolution based coordinates
//!

mod color;
mod frame;

use cgmath::vec2;
use ngon::{alias::Vec2, tris::Tris};
use piston_window::*;

/// Application main loop
fn main() {
    let mut window: PistonWindow = WindowSettings::new("ngon", (1920, 1080))
        .samples(1)
        .exit_on_esc(true)
        .build()
        .unwrap_or_else(|e| panic!("error creating main window: {}", e));
    let mut state = frame::State::new();
    let mut tris = Tris::new();

    while let Some(e) = window.next() {
        let dims = window.size();
        tris.clear();

        frame::frame(dims, &mut state, &mut tris);

        window.draw_2d(&e, |_, g, _| {
            clear([0.1, 0.1, 0.1, 1.0], g);

            Coord::new(dims).convert(&mut tris);
            g.tri_list_c(&Default::default(), |f| f(&tris.tr, &tris.cl));
        });
    }
}

// Cache the data needed to convert coordinates
struct Coord {
    hdims: Vec2,
}
impl Coord {
    fn new(dims: Size) -> Self {
        Self {
            hdims: vec2(dims.width as f32 * 0.5, dims.height as f32 * 0.5),
        }
    }
    // Convert the given point from our space into Piston window coordinates
    fn to(&self, p: Vec2) -> Vec2 {
        vec2(p.x / self.hdims.x, p.y / self.hdims.y)
    }
    // Convert the contents of the given tri buffer `in loco` to normal coords
    fn convert(&self, tris: &mut Tris) {
        tris.tr.iter_mut().for_each(|p| {
            let pp = self.to(vec2(p[0], p[1]));
            *p = [pp[0], pp[1]];
        })
    }
}
