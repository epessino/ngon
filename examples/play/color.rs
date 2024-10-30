//
//  Color utilities
//

use cgmath::vec4;
use ngon::alias::{Color, Vec4};

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

/// Convert an `sargb` 32-bit color into a  linear space `Color`
pub fn color_from_sargb(argb: u32) -> Color {
    srgb_to_linear(vec4(
        ((argb & 0x00_ff_00_00) >> 16) as f32 / 255.0,
        ((argb & 0x00_00_ff_00) >> 8) as f32 / 255.0,
        (argb & 0x00_00_00_ff) as f32 / 255.0,
        ((argb & 0xff_00_00_00) >> 24) as f32 / 255.0,
    ))
}

/// Converts color gamma from `sargb` to linear
pub fn srgb_to_linear(c: Color) -> Color {
    vec4(
        component_srgb_to_linear(c[0]),
        component_srgb_to_linear(c[1]),
        component_srgb_to_linear(c[2]),
        c[3],
    )
}

/// Converts color gamma from linear to `sargb`
pub fn linear_to_srgb(c: Color) -> Color {
    vec4(
        component_linear_to_srgb(c[0]),
        component_linear_to_srgb(c[1]),
        component_linear_to_srgb(c[2]),
        c[3],
    )
}

#[inline(always)]
fn component_srgb_to_linear(f: f32) -> f32 {
    if f <= 0.04045 {
        f / 12.92
    } else {
        ((f + 0.055) / 1.055).powf(2.4)
    }
}

#[inline(always)]
fn component_linear_to_srgb(f: f32) -> f32 {
    if f <= 0.0031308 {
        f * 12.92
    } else {
        1.055 * f.powf(1.0 / 2.4) - 0.055
    }
}

/// Return the vector (color) representation of the given `argb` - this only changes the format,
/// without color space conversion
#[inline(always)]
pub fn argb_to_color(argb: u32) -> Color {
    vec4(
        ((argb & 0x00_ff_00_00) >> 16) as f32 / 255.0,
        ((argb & 0x00_00_ff_00) >> 8) as f32 / 255.0,
        (argb & 0x00_00_00_ff) as f32 / 255.0,
        ((argb & 0xff_00_00_00) >> 24) as f32 / 255.0,
    )
}
/// Return the `argb` (u32) representation of the given linear color - this only changes the format,
/// without color space conversion
#[inline(always)]
pub fn color_to_argb(c: Color) -> u32 {
    (((c.w * 255.0) as u32) << 24)
        | (((c.z * 255.0) as u32) << 16)
        | (((c.y * 255.0) as u32) << 8)
        | ((c.x * 255.0) as u32)
}
/// Return the [r, g, b, a] representation of the given linear color - this only changes the format,
/// without color space conversion
#[inline(always)]
pub fn color_to_rgba(c: Color) -> [u8; 4] {
    [
        (c.x * 255.0) as u8,
        (c.y * 255.0) as u8,
        (c.z * 255.0) as u8,
        (c.w * 255.0) as u8,
    ]
}
