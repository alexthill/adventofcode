use crate::Solution;

pub const SOLUTION: Solution = Solution::U32((9241, 14610));

const LOOKUP: [(u32, u32); 9] = [
    (4, 3), (8, 4), (3, 8),
    (1, 1), (5, 5), (9, 9),
    (7, 2), (2, 6), (6, 7),
];

pub fn solve(input: String) -> Solution {
    input.as_bytes().chunks_exact(4).fold((0, 0), |mut acc, line| {
        let a = line[0] - b'A';
        let b = line[2] - b'X';
        let value = LOOKUP[(a * 3 + b) as usize];
        acc.0 += value.0;
        acc.1 += value.1;
        acc
    }).into()
}
