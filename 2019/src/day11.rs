use super::intcode_computer::Comp;
use aoc_lib_rust::{Day, Example, Solution};
use std::collections::HashMap;

pub struct Day11;

impl Day for Day11 {

    const PART1: Solution = Solution::U32(2478);
    const PART2: Solution = Solution::Str("
.X..X..XX..XXXX.XXX..X..X..XX...XX..XXXX...
.X..X.X..X....X.X..X.X..X.X..X.X..X....X...
.XXXX.X......X..X..X.X..X.X....X..X...X....
.X..X.X.....X...XXX..X..X.X.XX.XXXX..X.....
.X..X.X..X.X....X.X..X..X.X..X.X..X.X......
.X..X..XX..XXXX.X..X..XX...XXX.X..X.XXXX...");

    fn solve(input: &str) -> [Solution; 2] {
        let prog = Comp::parse_prog(input);

        fn paint(mut comp: Comp, map: &mut HashMap<(i16, i16), i64>) {
            let mut dir = (0, -1);
            let mut pos = (0, 0);

            comp.exec();
            while !comp.halted() {
                *map.entry(pos).or_insert(0) = comp.output().unwrap();
                comp.exec();
                dir = match comp.output().unwrap() {
                    0 => (dir.1, -dir.0),
                    1 => (-dir.1, dir.0),
                    _ => unreachable!(),
                };
                pos.0 += dir.0;
                pos.1 += dir.1;
                comp.push_input(map.get(&pos).copied().unwrap_or(0));
                comp.exec();
            }
        }

        let mut map = HashMap::new();
        paint(Comp::new(prog.clone(), [0]), &mut map);
        let sol1 = map.len();

        map.clear();
        map.insert((0, 0), 1);
        paint(Comp::new(prog.clone(), [1]), &mut map);

        let mut x_min = i16::MAX;
        let mut x_max = i16::MIN;
        let mut y_min = i16::MAX;
        let mut y_max = i16::MIN;
        for (x, y) in map.keys().copied() {
            x_min = x_min.min(x);
            x_max = x_max.max(x);
            y_min = y_min.min(y);
            y_max = y_max.max(y);
        }

        let mut sol2 = String::new();
        for y in y_min..=y_max {
            sol2.push('\n');
            for x in x_min..=x_max {
                match map.get(&(x, y)) {
                    Some(1) => sol2.push('X'),
                    _ => sol2.push('.'),
                }
            }
        }

        [Solution::U32(sol1 as _), Solution::String(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[];
}
