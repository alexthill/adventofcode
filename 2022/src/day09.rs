use crate::Solution;
use crate::utils::iter_parse_u32;

pub const SOLUTION: Solution = Solution::U32((5874, 2467));

// these limits are hardcoded for my inputs and should be calculated at runtime
const X_MIN: i32 = -139;
const X_MAX: i32 = 86;
const Y_MIN: i32 = -476;
const Y_MAX: i32 = 6;
const MAP_W: i32 = X_MAX - X_MIN + 1;
const MAP_H: i32 = Y_MAX - Y_MIN + 1;

pub fn solve(input: String) -> Solution {
    let input = input.as_bytes();
    let mut iter = input.iter().copied();
    
    let mut knots: [(i32, i32); 10] = [(-X_MIN, -Y_MIN); 10];
    
    let mut count1 = 1;
    let mut count2 = 1;
    let mut map = Vec::new();
    map.resize((MAP_H * MAP_W) as usize, 0);
    map[(-X_MIN + -Y_MIN * MAP_W) as usize] = 0b11;
    
    while let Some(dir) = iter.next() {
        iter.nth(0).unwrap();
        let num = iter_parse_u32(&mut iter).0.unwrap();
        for _ in 0..num {
            match dir {
                b'L' => knots[0].0 -= 1,
                b'R' => knots[0].0 += 1,
                b'U' => knots[0].1 += 1,
                b'D' => knots[0].1 -= 1,
                other => panic!("unknown direction {}", char::from_u32(other as u32).unwrap()),
            }
            
            let mut head = 0;
            let mut tail = 1;
            while tail < knots.len() {
                let dx = knots[tail].0 - knots[head].0;
                let dy = knots[tail].1 - knots[head].1;
                
                if dx.abs() == 2 || dy.abs() == 2 {
                    knots[tail].0 -= (dx > 0) as i32 - (dx < 0) as i32;
                    knots[tail].1 -= (dy > 0) as i32 - (dy < 0) as i32;
                } else {
                    break;
                }
                
                head += 1;
                tail += 1;
            }
            
            if tail > 1 {
                let idx = (knots[1].0 + knots[1].1 * MAP_W) as usize;
                if map[idx] & 0b01 == 0 {
                    map[idx] |= 0b01;
                    count1 += 1;
                }
                if tail == knots.len() {
                    let idx = (knots[9].0 + knots[9].1 * MAP_W) as usize;
                    if map[idx] & 0b10 == 0 {
                        map[idx] |= 0b10;
                        count2 += 1;
                    }
                }
            }
        }
    }
    
    (count1, count2).into()
}
