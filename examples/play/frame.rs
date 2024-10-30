//!
//! # Handles and modifies the draw state for each frame
//!

use crate::color::{as_vec4, col_lerp, color_from_sargb};
use ngon::{
    alias::{Color, Dec2},
    draw::{CapStyle, Draw, DrawParms},
    n_gon,
    tris::Tris,
};

use modulator::sources::{ScalarGoalFollower, ScalarSpring, Wave};
use modulator::ModulatorEnv;

use cgmath::{vec2, Rad, Rotation2};
use rand::prelude::*;
use std::{f32, time::Instant};

use piston_window::Size;

/// Compute one frame
pub fn frame(dims: Size, state: &mut State, tris: &mut Tris) {
    single_lines(dims, state, tris);
    complex_sweeps(dims, state, tris);
    n_gons(dims, state, tris);

    penta_dance(dims, state, tris);
    tri_dance(dims, state, tris);

    state.end_frame();
}

fn single_lines(_: Size, state: &mut State, tris: &mut Tris) {
    let mut lp = DrawParms::new();

    let rot = state.m.value("follow0") * 0.7;
    let rst = f32::consts::PI / 3.0;

    let center = vec2(-700.0, 270.0);
    let halfl = 200.0 * 0.5;

    // First line
    lp.color = [state.theme[8], state.theme[6]];
    lp.thickness = [4.0, 28.0];
    lp.cap = [CapStyle::Round(16), CapStyle::Round(32)];

    let (sn, cs) = rot.sin_cos();
    let dir = vec2(cs - sn, cs + sn);
    let l0 = [
        vec2(center.x - dir.x * halfl, center.y - dir.y * halfl),
        vec2(center.x + dir.x * halfl, center.y + dir.y * halfl),
    ];
    Draw::line(l0, &lp, tris);

    // Second line
    lp.color = [state.theme[1], state.theme[2]];
    lp.color[0][3] = 0.5;
    lp.color[1][3] = 0.5;
    lp.thickness = [16.0, (state.m.value("wave") * 8.0).abs()];
    lp.cap = [CapStyle::Square(0.0), CapStyle::Round(32)];

    let (sn, cs) = (rot + rst).sin_cos();
    let dir = vec2(cs - sn, cs + sn);
    let l1 = [
        vec2(center.x - dir.x * halfl, center.y - dir.y * halfl),
        vec2(center.x + dir.x * halfl, center.y + dir.y * halfl),
    ];
    Draw::line(l1, &lp, tris);

    // Third line
    lp.color = [state.theme[9], state.theme[11]];
    lp.color[0][3] = 0.5;
    lp.color[1][3] = 0.5;
    lp.cap = [CapStyle::Round(16), CapStyle::Square(0.0)];
    lp.thickness = [
        state.m.value("follow4") * 60.0,
        state.m.value("follow3") * 60.0,
    ];

    let (sn, cs) = (rot + rst * 2.0).sin_cos();
    let dir = vec2(cs - sn, cs + sn);
    let l1 = [
        vec2(center.x - dir.x * halfl, center.y - dir.y * halfl),
        vec2(center.x + dir.x * halfl, center.y + dir.y * halfl),
    ];
    Draw::line(l1, &lp, tris);
}

fn complex_sweeps(_: Size, state: &mut State, tris: &mut Tris) {
    let mut lp0 = DrawParms::new();

    lp0.thickness = [30.0, 30.0];
    lp0.color = [state.theme[2], state.theme[6]];

    let o0 = vec2(0.0, 200.0);
    let o1 = o0;

    let r = state.m.value("follow9") * 0.6;

    let r0 = cgmath::Matrix2::from_angle(Rad(r * 0.3));
    let r1 = cgmath::Matrix2::from_angle(Rad(r * -0.6));

    lp0.thickness[0] = 30.0 + state.m.value("follow0").abs() * 4.0;
    lp0.thickness[1] = 30.0;

    let line0 = [
        o0 + r0 * (vec2(o0[0] - 300.0, o0[1]) - o0),
        o0 + r0 * (vec2(o0[0] - 80.0, o0[1]) - o0),
    ];
    let line1 = [
        o1 + r1 * (vec2(o0[0], o0[1] + 80.0) - o1),
        o1 + r1 * (vec2(o0[0], o0[1] + 400.0) - o1),
    ];

    let lp1 = DrawParms {
        thickness: [30.0, 30.0 + state.m.value("wave") * 3.0],
        color: [state.theme[3], state.theme[7]],
        connect_subs: 0,
        ..lp0
    };
    Draw::lines(&[line0, line1], &[lp0, lp1], tris);
}

fn n_gons(_: Size, state: &mut State, tris: &mut Tris) {
    let dp = DrawParms::new()
        .color([state.theme[6], state.theme[9]])
        .thickness([12.0, 12.0])
        .connect_subs(16);
    let trs = Dec2 {
        scale: 160.0 + state.m.value("follow2") * 16.0,
        disp: vec2(-760.0, -300.0),
        rot: Rotation2::from_angle(Rad(state.m.value("follow1") * 30.0)),
    };
    let sides = 4;
    let lines = n_gon(sides, state.m.value("follow3"), &trs, None);
    Draw::lines_filled(&lines, &vec![dp; sides], tris);

    let dp = DrawParms::new()
        .color([state.theme[7], state.theme[10]])
        .thickness([
            4.0 + state.m.value("follow4") * 12.0,
            4.0 + state.m.value("follow5") * 12.0,
        ])
        .connect_subs(16);
    let trs = Dec2 {
        scale: 150.0,
        disp: vec2(-380.0, -300.0),
        rot: Rotation2::from_angle(Rad(state.m.value("follow6") * 30.0)),
    };
    let sides = 7;
    let lines = n_gon(sides, state.m.value("follow7"), &trs, None);
    Draw::lines_filled(&lines, &vec![dp; sides], tris);

    let dp = DrawParms::new()
        .color([state.theme[8], state.theme[11]])
        .thickness([12.0, 12.0])
        .connect_subs(16);
    let trs = Dec2 {
        scale: 160.0 + state.m.value("follow8") * 16.0,
        disp: vec2(0.0, -300.0),
        rot: Rotation2::from_angle(Rad(state.m.value("follow9") * 30.0)),
    };
    let sides = 3;
    let lines = n_gon(sides, state.m.value("follow10"), &trs, None);
    Draw::lines(&lines, &&vec![dp; sides], tris);

    let dp = DrawParms::new()
        .color([state.theme[9], state.theme[12]])
        .thickness([
            4.0 + state.m.value("follow11") * 12.0,
            4.0 + state.m.value("follow12") * 12.0,
        ])
        .connect_subs(16);
    let trs = Dec2 {
        scale: 150.0,
        disp: vec2(380.0, -300.0),
        rot: Rotation2::from_angle(Rad(state.m.value("follow13") * 30.0)),
    };
    let sides = 11;
    let lines = n_gon(sides, state.m.value("follow14"), &trs, None);
    Draw::lines(&lines, &&vec![dp; sides], tris);

    let dp = DrawParms::new()
        .color([state.theme[1], state.theme[3]])
        .thickness([
            1.0 + state.m.value("follow17") * 30.0,
            1.0 + state.m.value("follow18") * 30.0,
        ])
        .connect_subs(16);
    let trs = Dec2 {
        scale: 40.0 + state.m.value("follow15") * 64.0,
        disp: vec2(760.0, -300.0),
        rot: Rotation2::from_angle(Rad(state.m.value("follow16") * 30.0)),
    };
    let sides = 6;
    let lines = n_gon(sides, 1.0, &trs, None);
    Draw::lines(&lines, &vec![dp; sides], tris);
}

/// Generate a 5-gon primitive and then animate its parameters
fn penta_dance(_: Size, state: &mut State, tris: &mut Tris) {
    let c0 = 1.0 + state.m.value("follow19") * (state.theme.len() - 1) as f32;
    let c1 = 1.0 + state.m.value("follow20") * (state.theme.len() - 1) as f32;

    let dp = DrawParms::new()
        .color([state.blended_theme_color(c0), state.blended_theme_color(c1)])
        .thickness([2.0, 2.0])
        .connect_subs(16);
    let trs = Dec2 {
        scale: 50.0 + 200.0 * state.m.value("follow8"),
        disp: vec2(570.0, 200.0),
        rot: Rotation2::from_angle(Rad(state.m.value("follow23"))),
    };

    let sides = 5;
    let lines = n_gon(sides, 1.0, &trs, None);
    Draw::lines_filled(&lines, &vec![dp; sides], tris);
}
/// Generate a 3-gon primitive and then animate its parameters
fn tri_dance(_: Size, state: &mut State, tris: &mut Tris) {
    let c0 = 1.0 + state.m.value("follow25") * (state.theme.len() - 1) as f32;
    let c1 = 1.0 + state.m.value("follow26") * (state.theme.len() - 1) as f32;

    let dp = DrawParms::new()
        .color([state.blended_theme_color(c0), state.blended_theme_color(c1)])
        .thickness([
            2.0 + 15.0 * state.m.value("follow27"),
            2.0 + 15.0 * state.m.value("follow28"),
        ])
        .connect_subs(16);
    let trs = Dec2 {
        scale: 200.0 * state.m.value("follow29") * 0.5,
        disp: vec2(570.0, 200.0),
        rot: Rotation2::from_angle(Rad(state.m.value("wave"))),
    };

    let sides = 21;
    let lines = n_gon(sides, 0.85 * state.m.value("follow30"), &trs, None);
    Draw::lines(&lines, &vec![dp; sides], tris);
}

/// Container for application state data
pub struct State {
    /// Environment for animating modulators
    m: ModulatorEnv<f32>,
    /// A selection of colors for general use
    theme: [Color; 14],
    /// Last recorded moment in time
    earlier: Instant,
}

impl State {
    pub fn new() -> Self {
        // Create and populate the modulator environment
        let mut m = ModulatorEnv::new();

        let mut fol = Box::new(ScalarGoalFollower::new(Box::new(ScalarSpring::new(
            0.4, 0.5, 0.0,
        ))));
        fol.regions.push([-8.0, 8.0]);
        m.take("follow0", fol);

        for i in 1..=30 {
            let mut f = Box::new(ScalarGoalFollower::new(Box::new(ScalarSpring::new(
                1.0,
                0.0,
                rand::thread_rng().gen_range(0.0..1.0),
            ))));

            f.regions.push([0.0, 1.0]);
            f.pause_range = [0, 1000000];

            m.take(&format!("follow{}", i), f);
        }

        let mut fol = Box::new(ScalarGoalFollower::new(Box::new(ScalarSpring::new(
            1.0, 0.7, 0.0,
        ))));
        fol.regions.push([-10.0, 10.0]);
        m.take("follow9", fol);

        let wave = Wave::new(1.0, 0.3).wave(Box::new(|w, t| {
            (t * w.frequency * f32::consts::PI * 2.0).sin() * w.amplitude
        }));
        m.take("wave", Box::new(wave));

        // Make a goal follower using a spring, use to modulate the amp of the sin wave
        let mut amp_mod = Box::new(ScalarGoalFollower::new(Box::new(ScalarSpring::new(
            1.0, 0.0, 1.0,
        ))));
        amp_mod.regions.push([0.0, 12.0]);
        m.take("amp_mod", amp_mod); // modulates amp of waveform

        State {
            m,
            theme: Self::color_theme(),
            earlier: Instant::now(),
        }
    }

    /// End of frame processing
    pub fn end_frame(&mut self) {
        let ampmod = self.m.value("amp_mod");
        if let Some(sw) = self.m.get_mut("wave") {
            if let Some(ss) = sw.as_any().downcast_mut::<Wave>() {
                ss.amplitude = 1.0 + ampmod;
            }
        }

        self.m.advance(Self::time_delta(&mut self.earlier));
    }

    /// Get a theme color as a blend of two adjacent indices - notice that `at` defines
    /// the index and its fractional value
    pub fn blended_theme_color(&self, at: f32) -> Color {
        let i = at as usize;
        let j = if i < self.theme.len() - 1 { i + 1 } else { 1 };
        let f = at - (i as f32);

        col_lerp(f, as_vec4(self.theme[i]), as_vec4(self.theme[j]))
    }

    /// Create and return a color theme table
    fn color_theme() -> [Color; 14] {
        [
            color_from_sargb(0xff_24_24_2e), // background
            color_from_sargb(0xff_be_be_ef), // darkSPACE: main colors (5 shades, bright to dim)
            color_from_sargb(0xff_86_86_cb),
            color_from_sargb(0xff_72_72_a1),
            color_from_sargb(0xff_5b_5b_7b),
            color_from_sargb(0xff_49_49_5a),
            color_from_sargb(0xff_fe_77_34), // darkSPACE: sub color (3 shades, bright to dim)
            color_from_sargb(0xff_b0_68_45),
            color_from_sargb(0xff_64_45_40),
            color_from_sargb(0xff_dd_f8_dd), // darkFOREST: more colors (5 shades, bright to dim)
            color_from_sargb(0xff_a9_bc_a9),
            color_from_sargb(0xff_86_98_86),
            color_from_sargb(0xff_73_82_73),
            color_from_sargb(0xff_58_5f_58),
        ]
    }

    /// Advance time and update the `earlier` time, returns elapsed microseconds
    fn time_delta(earlier: &mut Instant) -> u64 {
        let now = Instant::now();
        let dt = ModulatorEnv::<f32>::duration_to_micros(now.duration_since(*earlier));

        *earlier = now;
        dt
    }
}
