use crate::Solution;
use crate::utils::iter_parse_u32;

pub const SOLUTION: Solution = Solution::U32((644, 27324));

pub fn solve(input: String) -> Solution {
    const SAND_START_X: u32 = 500;
    
    let mut iter = input.as_bytes().iter().copied();
    let mut parsed = Vec::new();
    let mut x_min = SAND_START_X;
    let mut x_max = SAND_START_X;
    let mut y_max = 0;
    
    loop {
        if let Some(x) = iter_parse_u32(&mut iter).0 {
            let (y, next) = iter_parse_u32(&mut iter);
            let y = y.unwrap();
            
            x_min = x_min.min(x);
            x_max = x_max.max(x);
            y_max = y_max.max(y);
            parsed.push((x, y));
            
            match next {
                Some(b' ') => assert_eq!(iter.nth(2).unwrap(), b' '),
                Some(b'\n') => parsed.push((0, 0)),
                other => panic!("unexpected char {:?} after number", other),
            }
        } else {
            break;
        }
    }
    
    // two extra columns left and right, one for the sand to fall down and one for a wall
    x_min -= 2;
    x_max += 2;
    // two extra rows for the implicit bottom
    let h = y_max + 2;
    let w = x_max - x_min + 1;
    // input should be about the size of the needed map, so just reuse the allocation
    let mut map = input.into_bytes();
    map.clear();
    map.resize((w * h) as usize, 0);
    map.resize((w * h + w) as usize, 1);
    
    for i in 1..=h {
        map[(i * w - 1) as usize] = 1;
        map[(i * w) as usize] = 1;
    }
    
    let mut i = 0;
    while i < parsed.len() - 1 {
        let s = parsed[i];
        let e = parsed[i + 1];
        if e == (0, 0) {
            i += 2;
        } else {
            if s.0 == e.0 {
                if s.1 < e.1 {
                    for y in s.1..=e.1 {
                        map[(s.0 - x_min + y * w) as usize] = 1;
                    }
                } else {
                    for y in e.1..=s.1 {
                        map[(s.0 - x_min + y * w) as usize] = 1;
                    }
                }
            } else if s.1 == e.1 {
                if s.0 < e.0 {
                    for x in s.0..=e.0 {
                        map[(x - x_min + s.1 * w) as usize] = 1;
                    }
                } else {
                    for x in e.0..=s.0 {
                        map[(x - x_min + s.1 * w) as usize] = 1;
                    }
                }
            } else {
                panic!("found diagonal line");
            }
            
            i += 1;
        }
    }
    
    let left = h - (SAND_START_X - x_min);
    let right = h - (x_max - SAND_START_X);
    let triangle_left = left * (left + 1) / 2;
    let triangle_right = right * (right + 1) / 2;
    
    let mut count1: u32 = 0;
    let mut count2: u32 = triangle_left + triangle_right;
    let mut keep_going_part1 = true;
    let mut stack = Vec::with_capacity(h as usize);
    stack.push((SAND_START_X - x_min) as usize);
    
    while let Some(mut pos) = stack.pop() {
        loop {
            let mut new_pos = pos + w as usize;
            if (map[new_pos - 1] & map[new_pos] & map[new_pos + 1]) != 0 {
                break;
            }
            if map[new_pos] != 0 {
                new_pos -= 1;
                if map[new_pos] != 0 {
                    new_pos += 2;
                }
            }
            stack.push(pos);
            pos = new_pos;
        }
        if pos >= map.len() - 2 * w as usize {
            keep_going_part1 = false;
        }
        map[pos] = 1;
        count1 += keep_going_part1 as u32;
        count2 += 1;
    }
    
    (count1, count2).into()
}
