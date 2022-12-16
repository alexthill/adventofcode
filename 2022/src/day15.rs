use crate::Solution;
use crate::utils::iter_parse_i32;

pub const SOLUTION: Solution = Solution::U64((4724228, 13622251246513));

pub fn solve(input: String) -> Solution {
    const LINE_Y: i32 = 2000000;
    const LIMIT: i32 = 4000000;
    
    let mut ranges = Vec::new();
    let mut sensors = Vec::new();
    let mut beacons_at_line: Vec<(i32, i32)> = Vec::new();
    
    let mut iter = input.bytes();
    loop {
        if iter.nth("Sensor at x=".len() - 1).is_none() {
            break;
        }
        
        let sx = iter_parse_i32(&mut iter).0.unwrap();
        iter.nth(" y=".len() - 1).unwrap();
        let sy = iter_parse_i32(&mut iter).0.unwrap();
        iter.nth(" closest beacon is at x=".len() - 1).unwrap();
        let bx = iter_parse_i32(&mut iter).0.unwrap();
        iter.nth(" y=".len() - 1).unwrap();
        let by = iter_parse_i32(&mut iter).0.unwrap();
        
        let r = (sx - bx).abs() + (sy - by).abs();
        let dy = (sy - LINE_Y).abs();
        
        sensors.push((sx, sy, r));
        
        if dy <= r {
            ranges.push((sx - r + dy, sx + r - dy));
        }
        
        if by == LINE_Y && beacons_at_line.iter().all(|b| b.0 != bx) {
            beacons_at_line.push((bx, by));
        }
    }
    
    ranges.sort_unstable_by_key(|r| r.0);
    let mut last_max = ranges[0].1;
    let mut count = last_max - ranges[0].0 + 1;
    for i in 1..ranges.len() {
        if ranges[i].0 <= last_max {
            if ranges[i].1 > last_max {
                count += ranges[i].1 - last_max;
                last_max = ranges[i].1
            }
        } else {
            count += ranges[i].1 - ranges[i].0 + 1;
            last_max = ranges[i].1
        }
    }
    let count = count as u64 - beacons_at_line.len() as u64;
    
    let mut squares = vec![(LIMIT / 2, LIMIT / 2, LIMIT)];
    let mut pos = (0, 0);
    while let Some((x, y, r)) = squares.pop() {
        if sensors.iter().any(|(sx, sy, sr)| (sx - x).abs() + (sy - y).abs() <= sr - r) {
            continue;
        }
        
        if r == 0 {
            pos = (x as u64, y as u64);
            break;
        }

        let r = if r == 1 { 0 } else { (r + 1) / 2 };
        if y + r >= 0 && LIMIT >= y - r {
            if x >= 0 && LIMIT >= x - 2 * r {
                squares.push((x - r, y, r));
            }
            if x + 2 * r >= 0 && LIMIT >= x {
                squares.push((x + r, y, r));
            }
        }
        if x + r >= 0 && LIMIT >= x - r {
            if y >= 0 && LIMIT >= y - 2 * r {
                squares.push((x, y - r, r));
            }
            if y + 2 * r >= 0 && LIMIT >= y {
                squares.push((x, y + r, r));
            }
        }
    }
    let freq = pos.0 * 4000000 + pos.1;
    
    (count, freq).into()
}
