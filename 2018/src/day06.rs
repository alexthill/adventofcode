use aoc_lib_rust::{Day, Example, Solution};

pub struct Day06;

impl Day for Day06 {

    const PART1: Solution = Solution::U32(3647);
    const PART2: Solution = Solution::U32(41605);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let safe_dist = if example { 32 } else { 10000 };
        let mut points = input.lines().map(|line| {
            let mut parts = line.split(", ").map(|part| part.parse::<i16>().unwrap());
            (parts.next().unwrap(), parts.next().unwrap(), 0, false)
        }).collect::<Vec<_>>();

        let w = points.iter().max_by_key(|point| point.0).unwrap().0;
        let h = points.iter().max_by_key(|point| point.1).unwrap().1;

        let mut sol2 = 0;
        for y in 0..=h {
            for x in 0..=w {
                let mut min_d = i16::MAX;
                let mut best = None;
                let mut sum = 0;
                for (i, point) in points.iter().enumerate() {
                    let d = (point.0 - x).abs() + (point.1 - y).abs();
                    if d <= min_d {
                        best = if d == min_d { None } else { Some(i) };
                        min_d = d;
                    }
                    sum += d;
                }
                if let Some(idx) = best {
                    points[idx].2 += 1;
                    if x == 0 || y == 0 || x == w || y == h {
                        points[idx].3 = true;
                    }
                }
                if sum < safe_dist {
                    sol2 += 1;
                }
            }
        }
        let sol1 = points.iter()
            .filter(|point| !point.3)
            .max_by_key(|point| point.2)
            .unwrap().2;

        [Solution::U32(sol1 as _), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(17), Solution::U32(16)],
            input: "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9",
        },
    ];
}
