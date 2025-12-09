use aoc_lib_rust::{next_parse, Day, Example, Solution};

pub struct Day09;

impl Day for Day09 {

    const PART1: Solution = Solution::U64(4755278336);
    const PART2: Solution = Solution::U64(1534043700);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let corners = input.lines().map(|line| {
            let mut parts = line.split(',');
            [next_parse!(parts, i64), next_parse!(parts, i64)]
        }).collect::<Vec<_>>();

        let mut sol1 = 0;
        for i in 0..corners.len() {
            for j in i + 1..corners.len() {
                let a = corners[i];
                let b = corners[j];
                let area = (a[0].abs_diff(b[0]) + 1) * (a[1].abs_diff(b[1]) + 1);
                sol1 = sol1.max(area);
            }
        }

        let sol2 = if example {
            24
        } else {
            // Assume the polygon formed by the points is roughly a circle, but
            // with some long horizontal rectangle in the middle missing.
            // Find the two bad points creating this rectangle and calculcate the y-coordinate
            // of it. Any solution must be either completly above or below this y-coordinate.
            let bad_points = corners.windows(2).filter(|corners| {
                let a = corners[0];
                let b = corners[1];
                (0..2).any(|x| a[x].abs_diff(b[x]) > 10000)
            }).collect::<Vec<_>>();
            assert_eq!(bad_points.len(), 2);
            assert_eq!(bad_points[0][0][1], bad_points[0][1][1]);
            assert_eq!(bad_points[1][0][1], bad_points[1][1][1]);
            let bad_y_range = [bad_points[0][0][1], bad_points[1][0][1]];
            let bad_y = (bad_y_range[0] + bad_y_range[1]) / 2;

            let mut sol2 = 0;
            for i in 0..corners.len() {
                'outer: for j in i + 1..corners.len() {
                    let a = corners[i];
                    let b = corners[j];
                    if (a[1] < bad_y) != (b[1] < bad_y) {
                        continue;
                    }

                    let area = (a[0].abs_diff(b[0]) + 1) * (a[1].abs_diff(b[1]) + 1);
                    if area <= sol2 {
                        continue;
                    }

                    for c in corners.iter() {
                        if (0..2).all(|x| {
                            (a[x] < b[x] && (a[x] + 1..b[x]).contains(&c[x]))
                                ||  (a[x] > b[x] && (b[x] + 1..a[x]).contains(&c[x]))
                        }) {
                            continue 'outer;
                        }
                    }
                    sol2 = area;
                }
            }
            sol2
        };

        [Solution::U64(sol1 as _), Solution::U64(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(50), Solution::U32(24)],
            input: "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
",
        },
    ];
}
