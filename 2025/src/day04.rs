use aoc_lib_rust::{Day, Example, Solution};

pub struct Day04;

impl Day for Day04 {

    const PART1: Solution = Solution::U32(1547);
    const PART2: Solution = Solution::U32(8948);

    fn solve(input: &str) -> [Solution; 2] {
        let w = input.bytes().position(|ch| ch == b'\n').unwrap() + 2;
        let h = input.len() / w + 3;

        let mut map = vec![b'.'; w];
        for line in input.as_bytes().split(|ch| *ch == b'\n') {
            map.push(b'.');
            map.extend_from_slice(line);
            map.push(b'\n');
        }
        map.resize(w * h, b'.');

        let mut sol1 = 0;
        for y in 1..h - 1 {
            for x in 1..w - 1 {
                let pos = x + w * y;
                if map[pos] != b'@' {
                    continue;
                }

                let mut adj = 0;
                for dy in -1..=1 {
                    let apos = pos as i32 + w as i32 * dy;
                    for dx in -1..=1 {
                        if dx == 0 && dy == 0 {
                            continue;
                        }
                        adj += (map[(apos + dx) as usize] == b'@') as u32;
                    }
                }
                sol1 += (adj < 4) as u32;
            }
        }

        // This has been optimized.
        let mut sol2 = 0;
        let mut y = 1;
        while y < h - 1 {
            let mut removed = false;
            for x in 1..w - 1 {
                let pos = x + w * y;
                if map[pos] != b'@' {
                    continue;
                }

                let adj = (map[pos - w - 1] == b'@') as u32
                    + (map[pos - w    ] == b'@') as u32
                    + (map[pos - w + 1] == b'@') as u32
                    + (map[pos - 1    ] == b'@') as u32
                    + (map[pos + 1    ] == b'@') as u32
                    + (map[pos + w - 1] == b'@') as u32
                    + (map[pos + w    ] == b'@') as u32
                    + (map[pos + w + 1] == b'@') as u32;
                if adj < 4 {
                    map[pos] = b'.';
                    sol2 += 1;
                    removed = true;
                }
            }
            if removed {
                y -= 1;
            } else {
                y += 1;
            }
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(13), Solution::U32(43)],
            input: "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.",
        },
    ];
}
