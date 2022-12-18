use crate::Solution;
use crate::utils::iter_parse_u32;

pub const SOLUTION: Solution = Solution::U32((3390, 2058));

pub fn solve(input: String) -> Solution {
    let mut iter = input.bytes();
    let mut drops = Vec::new();
    let mut max = [0, 0, 0];
    while let Some(x) = iter_parse_u32(&mut iter).0 {
        let y = iter_parse_u32(&mut iter).0.unwrap();
        let z = iter_parse_u32(&mut iter).0.unwrap();
        let coords = [x + 1, y + 1, z + 1];
        for i in 0..3 {
            max[i] = max[i].max(coords[i]);
        }
        drops.push(coords);
    }
    
    // pad z on both sides with empty space but x and y only on one side
    // this is sufficient because we a use a 1D-array and over/underflowing
    // the coordinates for a row/plane will land in the next/previous row/plane
    // as long as it is a valid array index
    let len = [max[0] + 1, max[1] + 1, max[2] + 2];
    let sizes = [1, len[0] as usize, (len[0] * len[1]) as usize];
    let mut grid = Vec::new();
    grid.resize((len[0] * len[1] * len[2]) as usize, 0);
    
    for &[x, y, z] in drops.iter() {
        let idx = (z * len[0] * len[1] + y * len[0] + x) as usize;
        grid[idx] = 1;
    }
    
    let mut count1: u32 = 0;
    for &[x, y, z] in drops.iter() {
        let idx = (z * len[0] * len[1] + y * len[0] + x) as usize;
        for &size in sizes.iter() {
            if grid[idx - size] == 0 {
                count1 += 1;
            }
            if grid[idx + size] == 0 {
                count1 += 1;
            }
        }
    }
    
    let mut count2: u32 = 0;
    let mut queue = Vec::new();
    queue.push(0);
    grid[0] = 2;
    while let Some(idx) = queue.pop() {
        for &size in sizes.iter() {
            if idx + size < grid.len() - 1 {
                match grid[idx + size] {
                    0 => {
                        grid[idx + size] = 2;
                        queue.push(idx + size);
                    }
                    1 => count2 += 1,
                    _ => {}
                }
            }
            if idx > size {
                match grid[idx - size] {
                    0 => {
                        grid[idx - size] = 2;
                        queue.push(idx - size);
                    }
                    1 => count2 += 1,
                    _ => {}
                }
            }
        }
    }
    
    (count1, count2).into()
}
