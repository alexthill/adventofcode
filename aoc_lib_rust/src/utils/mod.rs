pub mod vector;

use std::cmp;
use std::ops;

pub fn gcd<T>(mut a: T, mut b: T) -> T
where
    T: Copy + Default + PartialEq + cmp::PartialOrd + ops::Rem<Output = T>,
{
    if a == b { return a; }
    if b > a {
        let temp = a;
        a = b;
        b = temp;
    }
    while b > T::default() {
        let temp = a;
        a = b;
        b = temp % b;
    }
    return a;
}

pub fn lcm<T>(a: T, b: T) -> T
where
    T: Copy + Default + PartialEq + cmp::PartialOrd + ops::Rem<Output = T>
        + ops::Div<Output = T> + ops::Mul<Output = T>,
{
    return a * (b / gcd(a, b));
}
