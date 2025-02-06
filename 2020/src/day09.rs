use aoc_lib_rust::{Day, Example, Solution};

pub struct Day09;

impl Day for Day09 {

    const PART1: Solution = Solution::U64(258585477);
    const PART2: Solution = Solution::U64(36981213);

    fn solve2(input: &str, is_example: bool) -> [Solution; 2] {
        let preamble_len = if is_example { 5 } else { 25 };
        let mut nums = Vec::new();
        let mut sol1 = 0;

        for line in input.as_bytes().split(|c| *c == b'\n') {
            let num = line.iter().fold(0, |acc, c| acc * 10 + (*c - b'0') as u64);
            if sol1 == 0 && nums.len() >= preamble_len {
                let mut found = false;
                'outer: for i in nums.len()-preamble_len..nums.len()-1 {
                    for j in i+1..nums.len() {
                        if nums[i] != nums[j] && nums[i] + nums[j] == num {
                            found = true;
                            break 'outer;
                        }
                    }
                }
                if !found {
                    sol1 = num;
                }
            }
            nums.push(num);
        }

        let mut sol2 = 0;
        'outer: for i in 0..nums.len()-1 {
            let mut sum = nums[i];
            for j in i+1..nums.len() {
                sum += nums[j];
                if sum > sol1 {
                    continue 'outer;
                }
                if sum == sol1 {
                    let slice = &mut nums[i..=j];
                    slice.sort_unstable();
                    sol2 = slice[0] + slice[slice.len() - 1];
                    break 'outer;
                }
            }
        }

        [Solution::U64(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(127), Solution::U32(62)],
            input: "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576",
        },
    ];
}
