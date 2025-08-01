use aoc_lib_rust::{Day, Example, Solution};

pub struct Day03;

impl Day for Day03 {

    const PART1: Solution = Solution::U32(430);
    const PART2: Solution = Solution::U32(312453);

    fn solve(input: &str) -> [Solution; 2] {
        let n = input.parse::<i32>().unwrap();

        let ring = ((n as f32).sqrt().ceil() as i32) / 2;
        let side_len = ring * 2 + 1;
        let prev_ring_value = (side_len - 2).pow(2);
        let offset = (n - prev_ring_value) % (side_len - 1);
        let dist_from_middle = (offset - ring).abs();
        let sol1 = ring + dist_from_middle;

        let mut values = vec![0, 1, 1, 2, 4, 5, 10, 11, 23, 25];
        let mut i = 9;
        let sol2 = loop {
            i += 1;
            let ring = ((i as f32).sqrt().ceil() as usize) / 2;
            let side_len = ring * 2 + 1;
            let prev = (side_len - 2).pow(2);
            let offset = (i - prev - 1) % (side_len - 1);
            let side = (i - prev - 1) / (side_len - 1);
            let d = side * 2;
            let prev = (ring + 1).pow(2) + (ring % 2);

            let val = values[i - 1] + match (side, offset) {
                (0, 0) => values[i - prev + 1],
                (0, 1) => values[i - 2] + values[i - prev] + values[i - prev + 1],
                (0, _) if offset == side_len - 2 => values[i - prev - 1],
                (0, _) if offset == side_len - 3 => values[i - prev - 1] + values[i - prev],
                (0, _) => values[i - prev - 1] + values[i - prev] + values[i - prev + 1],
                (_, 0) =>
                    values[i - 2] + values[i - prev - d] + values[i - prev - d + 1],
                (3, _) if offset == side_len - 2 => values[i - prev - d - 1] + values[i - prev - d],
                (3, _) if offset == side_len - 3 =>
                    values[i - prev - d - 1] + values[i - prev - d] + values[i - prev - d + 1],
                (_, _) if offset == side_len - 2 => values[i - prev - d - 1],
                (_, _) if offset == side_len - 3 => values[i - prev - d - 1] + values[i - prev - d],
                (_, _) =>
                    values[i - prev - d - 1] + values[i - prev - d] + values[i - prev - d + 1],
            };
            if val > n {
                break val;
            }
            values.push(val);
        };

        [Solution::U32(sol1 as _), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(3), Solution::None],
            input: "10",
        },
        Example {
            solution: [Solution::U32(2), Solution::None],
            input: "11",
        },
        Example {
            solution: [Solution::U32(3), Solution::None],
            input: "12",
        },
        Example {
            solution: [Solution::U32(2), Solution::None],
            input: "23",
        },
        Example {
            solution: [Solution::U32(4), Solution::None],
            input: "25",
        },
        Example {
            solution: [Solution::U32(31), Solution::None],
            input: "1024",
        },
    ];
}
