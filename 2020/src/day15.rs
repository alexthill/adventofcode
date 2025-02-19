use aoc_lib_rust::{Day, Example, Solution};

pub struct Day15;

impl Day for Day15 {

    const PART1: Solution = Solution::U32(1428);
    const PART2: Solution = Solution::U32(3718541);

    fn solve(input: &str) -> [Solution; 2] {
        let mut start_nums = input.split(',')
            .map(|s| s.parse().unwrap())
            .collect::<Vec<usize>>();
        let start_n = start_nums.pop().unwrap();
        let mut last_said = Vec::new();
        last_said.resize(*start_nums.iter().max().unwrap() + 1, 0);

        let mut i = 1;
        for &n in &start_nums {
            last_said[n] = i;
            i += 1;
        }

        let mut last_n = start_n;
        let mut sol1 = 0;
        for i in i..30000000 {
            if i == 2020 {
                sol1 = last_n;
            }
            let n = match last_said.get(last_n) {
                None | Some(0) => {
                    //println!("{}: {last_n} was not said before", i + 1);
                    0
                }
                Some(turn) => {
                    //println!("{}: {last_n} was last said {turn}", i + 1);
                    i - turn
                }
            };
            if last_n >= last_said.len() {
                last_said.resize(last_n + 1, 0);
            }
            last_said[last_n] = i;
            last_n = n;
        }
        let sol2 = last_n;

        [Solution::U32(sol1 as _), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(436), Solution::U32(175594)],
            input: "0,3,6",
        },
        Example {
            solution: [Solution::U32(1), Solution::U32(2578)],
            input: "1,3,2",
        },
        Example {
            solution: [Solution::U32(10), Solution::U32(3544142)],
            input: "2,1,3",
        },
    ];
}
