use std::cmp;
use std::iter;
use std::ops;

pub type Vec2 = Vector<f32, 2>;
pub type UVec2 = Vector<u32, 2>;
pub type IVec2 = Vector<i32, 2>;

/// A fixed sized vector that is generic over its type and size.
#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Vector<T, const N: usize>([T; N]);

impl<T, const N: usize> Vector<T, N> {
    /// Creates a vector from an array.
    pub fn new(array: [T; N]) -> Self {
        Self(array)
    }

    pub fn map<S, F: FnMut(T) -> S>(self, f: F) -> Vector<S, N> {
        Vector(self.0.map(f))
    }

    pub fn x_mut(&mut self) -> &mut T {
        &mut self.0[0]
    }

    pub fn y_mut(&mut self) -> &mut T {
        &mut self.0[1]
    }

    pub fn z_mut(&mut self) -> &mut T {
        &mut self.0[2]
    }

    pub fn w_mut(&mut self) -> &mut T {
        &mut self.0[3]
    }
}

impl<T: Copy, const N: usize> Vector<T, N> {
    pub fn x(&self) -> T {
        self.0[0]
    }

    pub fn y(&self) -> T {
        self.0[1]
    }

    pub fn z(&self) -> T {
        self.0[2]
    }

    pub fn w(&self) -> T {
        self.0[3]
    }
}

impl<T: iter::Sum, const N: usize> Vector<T, N> {
    pub fn sum(self) -> T {
        self.0.into_iter().sum()
    }
}

impl<T: cmp::Ord, const N: usize> Vector<T, N> {
    pub fn min(self) -> Option<T> {
        self.0.into_iter().min()
    }

    pub fn max(self) -> Option<T> {
        self.0.into_iter().max()
    }
}

impl<T: ops::AddAssign, const N: usize> ops::Add for Vector<T, N> {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}

impl<T: ops::AddAssign, const N: usize> ops::AddAssign for Vector<T, N> {
    fn add_assign(&mut self, rhs: Self) {
        for (a, b) in self.0.iter_mut().zip(rhs.0) {
            *a += b;
        }
    }
}

impl<T: ops::Mul<Output = T> + Copy, const N: usize> ops::Mul<T> for Vector<T, N> {
    type Output = Self;

    fn mul(self, rhs: T) -> Self::Output {
        Self(self.0.map(|a| a * rhs))
    }
}

impl<T: ops::SubAssign, const N: usize> ops::Sub for Vector<T, N> {
    type Output = Self;

    fn sub(mut self, rhs: Self) -> Self::Output {
        self -= rhs;
        self
    }
}

impl<T: ops::SubAssign, const N: usize> ops::SubAssign for Vector<T, N> {
    fn sub_assign(&mut self, rhs: Self) {
        for (a, b) in self.0.iter_mut().zip(rhs.0) {
            *a -= b;
        }
    }
}

impl<T: ops::Div<Output = T> + Copy, const N: usize> ops::Div<T> for Vector<T, N> {
    type Output = Self;

    fn div(self, rhs: T) -> Self::Output {
        Self(self.0.map(|a| a / rhs))
    }
}

impl<T, const N: usize> From<[T; N]> for Vector<T, N> {
    fn from(array: [T; N]) -> Self {
        Self(array)
    }
}

impl<T, const N: usize> From<Vector<T, N>> for [T; N] {
    fn from(vector: Vector<T, N>) -> Self {
        vector.0
    }
}

impl<T, const N: usize> ops::Index<usize> for Vector<T, N> {
    type Output = T;

    fn index(&self, idx: usize) -> &Self::Output {
        &self.0[idx]
    }
}

impl<T, const N: usize> ops::IndexMut<usize> for Vector<T, N> {
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        &mut self.0[idx]
    }
}

impl<T: Default + Copy, const N: usize> Default for Vector<T, N> {
    fn default() -> Self {
        Self([T::default(); N])
    }
}
