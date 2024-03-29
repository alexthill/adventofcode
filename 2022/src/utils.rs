use std::iter::Iterator;

/// Converts a str consisting only of ascii digits to an u32. If the str contains non-digit chars
/// or if the results overflows an u32, this function may panic or return a wrong result.
pub fn str_to_u32_fast(s: &str) -> u32 {
    s.bytes()
        .inspect(|c| debug_assert!(c.is_ascii_digit()))
        .map(|c| (c - b'0') as u32)
        .reduce(|acc, x| acc * 10 + x)
        .expect("unexpected empty string")
}

/// Calls next() on an iterator as long as it returns digits. Returns a tuple where the first entry
/// is the found number or None if the first call next() did not return a digit. The second entry
/// is the first non-digit found or None if the iterator returned None.
pub fn iter_parse_u32(iter: &mut impl Iterator<Item = u8>) -> (Option<u32>, Option<u8>) {
    if let Some(c) = iter.next() {
        if c.is_ascii_digit() {
            let mut acc = (c - b'0') as u32;
            for c in iter.by_ref() {
                if c.is_ascii_digit() {
                    acc = acc * 10 + (c - b'0') as u32;
                } else {
                    return (Some(acc), Some(c));
                }
            }
            (Some(acc), None)
        } else {
            (None, Some(c))
        }
    } else {
        (None, None)
    }
}

/// Calls next() on an iterator as long as it returns digits. Returns a tuple where the first entry
/// is the found number or None if the first call next() did not return a digit. The second entry
/// is the first non-digit found or None if the iterator returned None.
///
/// If the iterator first returns a '-' and than digits, a negative number will be returned.
/// If the '-' is directly followed by a non-digit, the function panics.
/// If the '-' is directly followed by None, (None, Some('-')) is returned.
pub fn iter_parse_i32(iter: &mut impl Iterator<Item = u8>) -> (Option<i32>, Option<u8>) {
    if let Some(c) = iter.next() {
        let (sign, mut acc) = if c == b'-' {
            match iter.next() {
                Some(c) if c.is_ascii_digit() => (-1, (c - b'0') as i32),
                Some(c) => panic!("expected '-' to be followed by digit, found {}", c),
                None => return (None, Some(c)),
            }
        } else if c.is_ascii_digit() {
            (1, (c - b'0') as i32)
        } else {
            return (None, Some(c));
        };
        
        for c in iter.by_ref() {
            if c.is_ascii_digit() {
                acc = acc * 10 + (c - b'0') as i32;
            } else {
                return (Some(acc * sign), Some(c));
            }
        }
        (Some(acc * sign), None)
    } else {
        (None, None)
    }
}
