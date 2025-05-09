use std::{cmp, mem, ops};

/// Calculates the GCD of two numbers with the euclidian algorithm.
pub fn gcd<T>(mut a: T, mut b: T) -> T
where
    T: Copy + Default + PartialEq + cmp::PartialOrd + ops::Rem<Output = T>,
{
    if a == b { return a; }
    if b > a {
        mem::swap(&mut a, &mut b);
    }
    while b > T::default() {
        mem::swap(&mut a, &mut b);
    }
    a
}

/// Calculates the LCM of two numbers with the euclidian algorithm.
pub fn lcm<T>(a: T, b: T) -> T
where
    T: Copy + Default + PartialEq + cmp::PartialOrd + ops::Rem<Output = T>
        + ops::Div<Output = T> + ops::Mul<Output = T>,
{
    a * (b / gcd(a, b))
}

/// Calculates the GCD and the Bézout coefficents of two numbers.
///
/// See <https://en.wikipedia.org/w/index.php?title=Extended_Euclidean_algorithm>.
pub fn extended_gcd<T>(a: T, b: T) -> (T, T, T)
where
    T: Copy + PartialEq + cmp::PartialEq + From<bool>
        + ops::Div<Output = T> + ops::Mul<Output = T> + ops::Sub<Output = T>,
{
    let zero = false.into();
    let one = true.into();
    let (mut old_r, mut r) = (a, b);
    let (mut old_s, mut s) = (one, zero);
    let (mut old_t, mut t) = (zero, one);

    while r != zero {
        let quotient = old_r / r;
        (old_r, r) = (r, old_r - quotient * r);
        (old_s, s) = (s, old_s - quotient * s);
        (old_t, t) = (t, old_t - quotient * t);
    }

    (old_r, old_s, old_t)
}

/// Calculates the multiplicative inverse modulo `n` of a number `a`.
///
/// See <https://en.wikipedia.org/w/index.php?title=Extended_Euclidean_algorithm>.
pub fn modular_inverse<T>(a: T, n: T) -> Option<T>
where
    T: Copy + PartialEq + cmp::PartialEq + cmp::PartialOrd + From<bool>
        + ops::Div<Output = T> + ops::Mul<Output = T>
        + ops::Add<Output = T> + ops::Sub<Output = T>,
{
    let zero = false.into();
    let one = true.into();
    let (mut old_t, mut t) = (zero, one);
    let (mut old_r, mut r) = (n, a);

    while r != zero {
        let quotient = old_r / r;
        (old_t, t) = (t, old_t - quotient * t);
        (old_r, r) = (r, old_r - quotient * r);
    }

    if old_r > one {
        None
    } else if old_t < zero {
        Some(old_t + n)
    } else {
        Some(old_t)
    }
}
