use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::utils::vector::IVec2;
use std::collections::VecDeque;

pub struct Day17;

impl Day for Day17 {

    const PART1: Solution = Solution::Str("RDURRDDLRD");
    const PART2: Solution = Solution::U32(526);

    fn solve(input: &str) -> [Solution; 2] {
        let mut sol1 = String::new();
        let mut sol2 = 0;
        let mut queue = VecDeque::new();
        queue.push_back((IVec2::new([0, 0]), 0, input.as_bytes().to_vec()));
        while let Some((pos, depth, path)) = queue.pop_front() {
            if pos == IVec2::new([3, 3]) {
                if sol1.is_empty() {
                    sol1 = String::from_utf8_lossy(&path[input.len()..]).into_owned();
                }
                sol2 = depth;
                continue;
            }

            let hash = md5::compute(&path);
            for (i, dir) in [[0, -1], [0, 1], [-1, 0], [1, 0]].into_iter().enumerate() {
                let pos = pos + dir.into();
                if !(0..4).contains(&pos.x()) || !(0..4).contains(&pos.y()) {
                    continue;
                }

                let h = if i.is_multiple_of(2) { hash[i / 2] >> 4 } else { hash[i / 2] & 0x0f };
                if h <= 0xa {
                    continue;
                }

                let mut path = path.clone();
                path.push([b'U', b'D', b'L', b'R'][i]);
                queue.push_back((pos, depth + 1, path));
            }
        }

        [Solution::String(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::Str("DDRRRD"), Solution::U32(370)],
            input: "ihgpwlah",
        },
        Example {
            solution: [Solution::Str("DDUDRLRRUDRD"), Solution::U32(492)],
            input: "kglvqrro",
        },
    ];
}
