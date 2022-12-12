use crate::Solution;

pub const SOLUTION: Solution = Solution::U32((383, 377));

pub fn solve(input: String) -> Solution {
    // we reuse the input as map and set the msb of each byte to 1 when it was visited
    const MASK: u8 = 0b10000000;
    
    let mut map = input.into_bytes();
    let w = 1 + map.iter().position(|&c| c == b'\n').unwrap();
    let start = map.iter().position(|&c| c == b'S').unwrap();
    let end = map.iter().position(|&c| c == b'E').unwrap();
    
    map[start] = b'a';
    map[end] = b'z';
    
    let mut distance = 0;
    let mut min_dist_to_height_a = u32::MAX;
    let mut queue_1 = Vec::with_capacity(w);
    let mut queue_2 = Vec::with_capacity(w);
    queue_1.push(end);
    
    'outer: loop {
        for pos in queue_1.drain(..) {
            if pos == start {
                break 'outer;
            }
            if map[pos] == (b'a' | MASK) && min_dist_to_height_a == u32::MAX {
                min_dist_to_height_a = distance;
            }
            
            if pos != 0 {
                let next = pos - 1;
                if (map[next] & MASK) == 0 && (map[pos] & !MASK) <= map[next] + 1 {
                    map[next] |= MASK;
                    queue_2.push(next);
                }
            }
            if pos >= w {
                let next = pos - w;
                if (map[next] & MASK) == 0 && (map[pos] & !MASK) <= map[next] + 1 {
                    map[next] |= MASK;
                    queue_2.push(next);
                }
            }
            if pos < map.len() - w {
                let next = pos + w;
                if (map[next] & MASK) == 0 && (map[pos] & !MASK) <= map[next] + 1 {
                    map[next] |= MASK;
                    queue_2.push(next);
                }
            }
            let next = pos + 1;
            if (map[next] & MASK) == 0 && (map[pos] & !MASK) <= map[next] + 1 {
                map[next] |= MASK;
                queue_2.push(next);
            }
        }
        
        if !queue_2.is_empty() {
            std::mem::swap(&mut queue_1, &mut queue_2);
            distance += 1;
        } else {
            distance = u32::MAX;
            break;
        }
    }
    
    (distance, min_dist_to_height_a).into()
}
