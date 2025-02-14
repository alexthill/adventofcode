use aoc_lib_rust::{Day, Example, Solution};

pub struct Day13;

impl Day for Day13 {

    const PART1: Solution = Solution::U32(1915);
    const PART2: Solution = Solution::None;

    fn solve(input: &str) -> [Solution; 2] {
        let mut lines = input.as_bytes().split(|c| *c == b'\n');
        let time = lines.next().unwrap().iter().fold(0, |acc, c| acc * 10 + (*c - b'0') as u32);

        let mut line2 = lines.next().unwrap().iter();
        let mut bus_id = 0;
        let mut best_id = u32::MAX;
        let mut best_time = u32::MAX;
        'outer: loop {
            while let Some(c) = line2.next() {
                if !c.is_ascii_digit() {
                    break;
                }
                bus_id = bus_id * 10 + (*c - b'0') as u32;
            }

            let t = bus_id - (time % bus_id);
            if t < best_time {
                best_time = t;
                best_id = bus_id;
            }

            loop {
                match line2.next() {
                    None => break 'outer,
                    Some(c) if c.is_ascii_digit() => {
                        bus_id = (*c - b'0') as u32;
                        continue 'outer;
                    }
                    _ => {}
                }
            }
        }

        [Solution::U32(best_time * best_id), Solution::None]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(295), Solution::U32(1068781)],
            input: "939
7,13,x,x,59,x,31,19",
        },
    ];
}
