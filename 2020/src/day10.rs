use aoc_lib_rust::{Day, Example, Solution};
use std::collections::HashMap;

pub struct Day10;

impl Day for Day10 {

    const PART1: Solution = Solution::U32(2080);
    const PART2: Solution = Solution::U64(6908379398144);

    fn solve(input: &str) -> [Solution; 2] {
        let mut adaptors = input.as_bytes().split(|c| *c == b'\n')
            .map(|line| line.iter().fold(0, |acc, c| acc * 10 + (*c - b'0') as u32))
            .collect::<Vec<u32>>();
        adaptors.sort_unstable();

        let diffs = adaptors.windows(2)
            .fold([0; 4], |mut acc, win| {
                let diff = win[1] - win[0];
                acc[diff as usize] += 1;
                acc
            });
        // do not forget to add diff from outlet to first adaptor
        // and diff from last adaptor to device
        let sol1 = (diffs[1] + 1) * (diffs[3] + 1);

        fn count_arrangements(
            curr: u32,
            mut adaptors: &[u32],
            cache: &mut HashMap<(u32, u32), u64>
        ) -> u64 {
            if let Some(&val) = cache.get(&(curr, adaptors[0])) {
                return val;
            }

            let mut total = 0;
            while !adaptors.is_empty() && adaptors[0] - curr <= 3 {
                let next = adaptors[0];
                adaptors = &adaptors[1..];
                if adaptors.is_empty() {
                    total += 1;
                } else {
                    let count = count_arrangements(next, adaptors, cache);
                    cache.insert((next, adaptors[0]), count);
                    total += count;
                }
            }
            total
        }

        let mut cache = HashMap::new();
        let sol2 = count_arrangements(0, &adaptors, &mut cache);

        [Solution::U32(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(4), Solution::U32(7)],
            input: "1
2
3
4",
        },
        Example {
            solution: [Solution::U32(35), Solution::U32(8)],
            input: "16
10
15
5
1
11
7
19
6
12
4",
        },
        Example {
            solution: [Solution::U32(220), Solution::U32(19208)],
            input: "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3",
        },
    ];
}
