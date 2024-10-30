//! # alias - renaming for convenience

pub type Vec2u = cgmath::Vector2<u32>;
pub type Vec3u = cgmath::Vector3<u32>;
pub type Vec4u = cgmath::Vector4<u32>;

pub type Vec2 = cgmath::Vector2<f32>;
pub type Vec3 = cgmath::Vector3<f32>;
pub type Vec4 = cgmath::Vector4<f32>;

pub type Mat2 = cgmath::Matrix2<f32>;
pub type Mat3 = cgmath::Matrix3<f32>;
pub type Mat4 = cgmath::Matrix4<f32>;

pub type Bas2 = cgmath::Basis2<f32>;
pub type Dec2 = cgmath::Decomposed<Vec2, Bas2>;

pub type Quat = cgmath::Quaternion<f32>;
pub type Color = cgmath::Vector4<f32>;

pub type Euler = cgmath::Euler<cgmath::Rad<f32>>;
