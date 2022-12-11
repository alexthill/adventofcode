use crate::Solution;
use crate::utils::iter_parse_u32;

pub const SOLUTION: Solution = Solution::U32((5874, 2467));

pub fn solve(input: String) -> Solution {
    // if we are careful the parsed instructions take less space (3 bytes) than the input (4-5 bytes)
    // so we can save an allocation by overwritting the input while pasing it
    let mut input = input.into_bytes();
    let mut x_min = 0;
    let mut x_max = 0;
    let mut y_min = 0;
    let mut y_max = 0;
    let mut pos = (0, 0);
    
    let mut read = 0;
    let mut write = 0;
    
    while read < input.len() {
        let dir = input[read];
        let num = iter_parse_u32(&mut input[read + 2..].iter().copied()).0.unwrap();
        let dir: (i8, i8) = match dir {
            b'L' => { pos.0 -= num as i32; x_min = x_min.min(pos.0); (-1,  0) },
            b'R' => { pos.0 += num as i32; x_max = x_max.max(pos.0); ( 1,  0) },
            b'U' => { pos.1 += num as i32; y_max = y_max.max(pos.1); ( 0,  1) },
            b'D' => { pos.1 -= num as i32; y_min = y_min.min(pos.1); ( 0, -1) },
            other => panic!("unknown direction {}", char::from_u32(other as u32).unwrap()),
        };
        
        read += if num < 10 { 4 } else if num < 100 { 5 } else { panic!("num of steps must be at most 99, found {}", num) };
        
        input[write] = num as u8;
        input[write + 1] = dir.0.to_ne_bytes()[0];
        input[write + 2] = dir.1.to_ne_bytes()[0];
        write += 3;
    }
    
    let w = x_max - x_min + 1;
    let h = y_max - y_min + 1;
    
    let mut count1 = 1;
    let mut count2 = 1;
    let mut knots: [(i32, i32); 10] = [(-x_min, -y_min); 10];
    let mut map = Vec::new();
    map.resize((h * w) as usize, 0);
    map[(-x_min + -y_min * w) as usize] = 0b11;
    
    for chunk in input[..write].chunks_exact(3) {
        for _ in 0..chunk[0] {
            knots[0].0 += i8::from_ne_bytes([chunk[1]]) as i32;
            knots[0].1 += i8::from_ne_bytes([chunk[2]]) as i32;
            
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
                let idx = (knots[1].0 + knots[1].1 * w) as usize;
                count1 += (map[idx] & 0b01 == 0) as u32;
                map[idx] |= 0b01;
                
                if tail == knots.len() {
                    let idx = (knots[9].0 + knots[9].1 * w) as usize;
                    count2 += (map[idx] & 0b10 == 0) as u32;
                    map[idx] |= 0b10;
                }
            }
        }
    }
    
    (count1, count2).into()
}
