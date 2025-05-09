use aoc_lib_rust::{Day, Example, Solution};

pub struct Day03;

impl Day for Day03 {

    const PART1: Solution = Solution::U32(116920);
    const PART2: Solution = Solution::U32(382);

    fn solve(input: &str) -> [Solution; 2] {
        let claims = input.lines().map(|line| {
            let mut parts = line.split(' ');

            let id = parts.next().unwrap()[1..].parse::<u32>().unwrap();
            assert_eq!(parts.next(), Some("@"));

            let mut parts_a = parts.next().unwrap().split([',', ':']);
            let left = parts_a.next().unwrap().parse::<u32>().unwrap();
            let top = parts_a.next().unwrap().parse::<u32>().unwrap();

            let mut parts_b = parts.next().unwrap().split('x');
            let w = parts_b.next().unwrap().parse::<u32>().unwrap();
            let h = parts_b.next().unwrap().parse::<u32>().unwrap();

            (id, left, top, left + w, top + h)
        }).collect::<Vec<_>>();
        let x_max = claims.iter().map(|claim| claim.3).max().unwrap();
        let y_max = claims.iter().map(|claim| claim.4).max().unwrap();

        let mut fabric = Vec::new();
        fabric.resize((x_max * y_max) as _, 0_u8);

        let mut sol1 = 0;
        for (_, left, top, right, bottom) in claims.iter().copied() {
            for y in top..bottom {
                for x in left..right {
                    let pos = (x + x_max * y) as usize;
                    sol1 += (fabric[pos] == 1) as u32;
                    fabric[pos] += 1;
                }
            }
        }

        let mut sol2 = 0;
        'claims: for (id, left, top, right, bottom) in claims {
            for y in top..bottom {
                for x in left..right {
                    let pos = (x + x_max * y) as usize;
                    if fabric[pos] != 1 {
                        continue 'claims;
                    }
                }
            }
            sol2 = id;
            break;
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(4), Solution::U32(3)],
            input: "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2",
        },
    ];
}
