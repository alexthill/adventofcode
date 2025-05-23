use aoc_lib_rust::{Day, Example, Solution};

pub struct Day05;

impl Day for Day05 {

    const PART1: Solution = Solution::U32(11108);
    const PART2: Solution = Solution::U32(5094);

    fn solve(input: &str) -> [Solution; 2] {
        let mut poly = input.as_bytes().to_vec();

        fn reduce(poly: &mut[u8]) -> usize {
            let mut i = 0;
            let mut j = 1;
            while j < poly.len() {
                let a = poly[i];
                let b = poly[j];
                if a.eq_ignore_ascii_case(&b) && a.is_ascii_lowercase() != b.is_ascii_lowercase() {
                    if i == 0 {
                        j += 1;
                        poly[i] = poly[j];
                    } else {
                        i -= 1;
                    }
                } else {
                    i += 1;
                    poly[i] = b;
                }
                j += 1;
            }
            i + 1
        }

        let sol1 = reduce(&mut poly);

        let mut poly_copy = Vec::with_capacity(sol1);
        let sol2 = (b'a'..=b'z').map(|rm| {
            poly_copy.clear();
            let iter = poly.iter().copied().take(sol1)
                .filter(|c| c.to_ascii_lowercase() != rm);
            poly_copy.extend(iter);
            reduce(&mut poly_copy)
        }).min().unwrap();

        [Solution::U32(sol1 as _), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(10), Solution::U32(4)],
            input: "dabAcCaCBAcCcaDA",
        },
    ];
}
