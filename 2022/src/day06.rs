use crate::Solution;

pub const SOLUTION: Solution = Solution::U32((1640, 3613));

pub fn solve(input: String) -> Solution {
    let input = input.as_bytes();
    let x = solve_for_window_len::<4>(&input);
    let y = solve_for_window_len::<14>(&input[x - 4..]) + x - 4;
    
    (x as u32, y as u32).into()
}

pub fn solve_for_window_len<const N: usize>(input: &[u8]) -> usize {
    let mut win_end = N;
    'outer: while win_end <= input.len() {
        // using rotate is faster than substracting b'a' and shifting
        let mut found = 1_u32.rotate_left(input[win_end - N] as u32);
        for &el in input[win_end - N + 1..win_end].iter() {
            let rot = 1_u32.rotate_left(el as u32);
            if (found & rot) == 0 {
                found |= rot;
            } else {
                if el != input[win_end - N] {
                    win_end += 2;
                } else {
                    win_end += 1;
                }
                continue 'outer;
            }
        }
        break;
    }
    win_end
}
