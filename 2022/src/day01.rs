use std::mem;
use crate::utils::iter_parse_u32;

pub fn solve(input: String) -> (u32, u32) {
    let mut iter = input.bytes();
    let mut top_three = [0; 3];
    let mut curr_cal = 0;
    
    loop {
        match iter_parse_u32(&mut iter) {
            (Some(num), _) => curr_cal += num,
            (None, Some(_)) => {
                try_insert(&mut top_three, curr_cal);
                curr_cal = 0;
            }
            _ => break,
        }
    }
    
    (top_three[0], top_three.into_iter().sum::<u32>())
}

fn try_insert(arr: &mut [u32; 3], val: u32) {
    let [a, b, c] = arr;
    if val > *c {
        *c = val;
        if val > *b {
            mem::swap(b, c);
            if val > *a {
                mem::swap(a, b);
            }
        }
    }
}
