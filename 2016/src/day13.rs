use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::utils::vector::IVec2;
use std::collections::{HashSet, VecDeque};

pub struct Day13;

impl Day for Day13 {

    const PART1: Solution = Solution::U32(92);
    const PART2: Solution = Solution::U32(124);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        fn is_open(coords: IVec2, num: i32) -> bool {
            let [x, y] = coords.into();
            let n = x*x + 3*x + 2*x*y + y + y*y + num;
            n.count_ones().is_multiple_of(2)
        }

        let num = input.parse::<i32>().unwrap();
        let start = IVec2::new([1, 1]);
        let end = if example {
            IVec2::new([7, 4])
        } else {
            IVec2::new([31, 39])
        };
        let mut map = HashSet::new();
        let mut queue = VecDeque::from([(start, 0)]);
        let mut sol1 = 0;
        let mut sol2 = 0;
        while let Some((pos, dist)) = queue.pop_front() {
            if dist == 51 && sol2 == 0 {
                sol2 = map.len();
            }
            if pos == end && sol1 == 0 {
                sol1 = dist;
            }
            if sol1 != 0 && sol2 != 0 {
                break;
            }

            let open = is_open(pos, num);
            if !open || map.contains(&pos) {
                continue;
            }
            map.insert(pos);
            for dir in [[1, 0], [-1, 0], [0, 1], [0, -1]] {
                let pos = pos + dir.into();
                if pos.x() >= 0 && pos.y() >= 0 {
                    queue.push_back((pos, dist + 1));
                }
            }
        }

        [Solution::U32(sol1), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(11), Solution::None],
            input: "10",
        },
    ];
}
