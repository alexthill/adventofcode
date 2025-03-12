use aoc_lib_rust::{Day, Example, Solution};

pub struct Day25;

impl Day for Day25 {

    const PART1: Solution = Solution::U32(4441893);
    const PART2: Solution = Solution::None;

    fn solve(input: &str) -> [Solution; 2] {
        const N: u64 = 20201227;

        let mut lines = input.lines();
        let key1: u64 = lines.next().unwrap().parse().unwrap();
        let key2: u64 = lines.next().unwrap().parse().unwrap();

        fn transform(subj: u64, loop_size: u64) -> u64 {
            if loop_size == 0 {
                1
            } else if loop_size % 2 == 0 {
                let num = transform(subj, loop_size / 2);
                (num * num) % N
            } else {
                let num = transform(subj, loop_size - 1);
                (num * subj) % N
            }
        }

        // loop_size for key2 is smaller than for key2
        // at least for my input...
        let mut loop_size = 0;
        let mut num = 1;
        for i in 1..1_000_000_000 {
            num = (num * 7) % N;
            if num == key2 {
                loop_size = i;
                break;
            }
        }
        let sol1 = transform(key1, loop_size);

        [Solution::U64(sol1), Solution::None]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(14897079), Solution::None],
            input: "5764801
17807724",
        },
    ];
}
