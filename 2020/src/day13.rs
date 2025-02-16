use aoc_lib_rust::{Day, Example, Solution};

pub struct Day13;

impl Day13 {
    /// see <https://en.wikipedia.org/w/index.php?title=Extended_Euclidean_algorithm>
    fn extended_gcd(a: i128, b: i128) -> (i128, i128, i128) {
        let (mut old_r, mut r) = (a, b);
        let (mut old_s, mut s) = (1, 0);
        let (mut old_t, mut t) = (0, 1);

        while r != 0 {
            let quotient = old_r / r;
            (old_r, r) = (r, old_r - quotient * r);
            (old_s, s) = (s, old_s - quotient * s);
            (old_t, t) = (t, old_t - quotient * t);
        }

        (old_r, old_s, old_t)
    }
}

impl Day for Day13 {

    const PART1: Solution = Solution::U32(1915);
    const PART2: Solution = Solution::U64(294354277694107);

    fn solve(input: &str) -> [Solution; 2] {
        let mut lines = input.as_bytes().split(|c| *c == b'\n');
        let time = lines.next().unwrap().iter().fold(0, |acc, c| acc * 10 + (*c - b'0') as u32);

        let mut i = 0;
        let mut line2 = lines.next().unwrap().iter();
        let mut bus_id = 0;
        let mut best_id = u32::MAX;
        let mut best_time = u32::MAX;
        let mut sol2 = 0;
        let mut bus_product = 1;
        'outer: loop {
            while let Some(c) = line2.next() {
                if !c.is_ascii_digit() {
                    debug_assert!(*c == b',');
                    break;
                }
                bus_id = bus_id * 10 + (*c - b'0') as u32;
            }

            {
                let bus_id = bus_id as i128;
                let (_, m1, m2) = Day13::extended_gcd(bus_product, bus_id);
                // chinese remainder theorem
                sol2 = i * m1 * bus_product + sol2 * m2 * bus_id;
                bus_product *= bus_id;
                sol2 = sol2 % bus_product;
            }

            i += 1;

            let t = bus_id - (time % bus_id);
            if t < best_time {
                best_time = t;
                best_id = bus_id;
            }

            loop {
                match line2.next() {
                    None => break 'outer,
                    Some(b',') => i += 1,
                    Some(c) if c.is_ascii_digit() => {
                        bus_id = (*c - b'0') as u32;
                        continue 'outer;
                    }
                    Some(other) => {
                        debug_assert!(*other == b'x');
                    }
                }
            }
        }

        [Solution::U32(best_time * best_id), Solution::U64(sol2.abs() as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(295), Solution::U32(1068781)],
            input: "939
7,13,x,x,59,x,31,19",
        },
    ];
}
