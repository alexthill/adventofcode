use aoc_lib_rust::{Day, Example, Solution};

pub struct Day18;

impl Day for Day18 {

    const PART1: Solution = Solution::U32(2035);
    const PART2: Solution = Solution::U32(20000577);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let mut row = input.bytes().filter_map(|ch| {
            match ch {
                b'.' => Some(true),
                b'^' => Some(false),
                _ => None,
            }
        }).collect::<Vec<_>>();
        let count1 = if example { 10 } else { 40 };
        let count2 = if example { 20 } else { 400_000 };

        let mut next = row.clone();
        let mut sol1 = 0;
        for _ in 0..count1 {
            for (i, cell) in next.iter_mut().enumerate() {
                let l = row.get(i.wrapping_sub(1)).copied().unwrap_or(true);
                let r = row.get(i.wrapping_add(1)).copied().unwrap_or(true);
                *cell = l == r;
                sol1 += row[i] as u32;
            }
            std::mem::swap(&mut row, &mut next);
        }
        let mut sol2 = sol1;
        for _ in count1..count2 {
            next[0] = row[1];
            for i in 1..row.len() - 1 {
                next[i] = row[i - 1] == row[i + 1];
            }
            next[row.len() - 1] = row[row.len() - 2];
            sol2 += row.iter().map(|cell| *cell as u32).sum::<u32>();
            std::mem::swap(&mut row, &mut next);
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(38), Solution::None],
            input: ".^^.^.^^^^",
        },
    ];
}
