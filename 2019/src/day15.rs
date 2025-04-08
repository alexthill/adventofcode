use super::intcode_computer::{Comp, Interrupt};
use aoc_lib_rust::{Day, Example, Solution};
use std::collections::{HashSet, VecDeque};

const MOVE_TO_DIR: [(i32, i32); 5] = [
    (0, 0), (0, -1), (0, 1), (-1, 0), (1, 0),
];
const MOVE_TO_REV: [usize; 5] = [0, 2, 1, 4, 3];
const W: usize = 100;
const H: usize = 100;
const START_POS: (i32, i32) = ((W / 2) as i32, (H / 2) as i32);

pub struct Day15;

impl Day for Day15 {

    const PART1: Solution = Solution::U32(294);
    const PART2: Solution = Solution::U32(388);

    fn solve(input: &str) -> [Solution; 2] {
        fn move_to(pos: (i32, i32), mov: usize) -> (i32, i32) {
            let dir = MOVE_TO_DIR[mov];
            (pos.0 + dir.0, pos.1 + dir.1)
        }

        let prog = Comp::parse_prog(input);
        let mut map = [[-1; W]; H];
        let mut pos = START_POS;
        let mut comp = Comp::new(prog, []);
        let mut path = Vec::new();
        let mut reachable_unknows = HashSet::from([1, 2, 3, 4].map(|mov| move_to(pos, mov)));
        let mut oxy_pos = (-1, -1);

        map[pos.1 as usize][pos.0 as usize] = 1;

        while reachable_unknows.len() != 0 {
            let mov = loop {
                if let Some(mov) = (1..=4).find(|&mov| {
                    let new_pos = move_to(pos, mov);
                    map[new_pos.1 as usize][new_pos.0 as usize] == -1
                }) {
                    break mov;
                }
                let mov = MOVE_TO_REV[path.pop().unwrap()];
                comp.push_input(mov as _);
                let Interrupt::Output(1) = comp.exec() else { unreachable!() };
                pos = move_to(pos, mov);
            };

            comp.push_input(mov as _);

            let Interrupt::Output(out) = comp.exec() else { unreachable!() };
            let new_pos = move_to(pos, mov);
            map[new_pos.1 as usize][new_pos.0 as usize] = out;
            reachable_unknows.remove(&new_pos);
            if out != 0 {
                path.push(mov);
                pos = new_pos;

                if out == 2 {
                    oxy_pos = new_pos;
                }

                for new_pos in (1..=4).map(|mov| move_to(pos, mov)) {
                    if map[new_pos.1 as usize][new_pos.0 as usize] == -1 {
                        reachable_unknows.insert(new_pos);
                    }
                }
            }
            //print_map(&map, pos);
        }

        let mut sol1 = 0;
        let mut queue = VecDeque::from([(0, START_POS)]);
        while let Some((d, pos)) = queue.pop_front() {
            if pos == oxy_pos {
                sol1 = d;
                break;
            }
            let cell = &mut map[pos.1 as usize][pos.0 as usize];
            if *cell == 0 || *cell == 42 {
                continue;
            }
            *cell = 42; // set this to some other value so we know we have visited it
            for new_pos in (1..=4).map(|mov| move_to(pos, mov)) {
                queue.push_back((d + 1, new_pos));
            }
        }

        let mut sol2 = 0;
        queue.clear();
        queue.push_back((0, oxy_pos));
        while let Some((d, pos)) = queue.pop_front() {
            let cell = &mut map[pos.1 as usize][pos.0 as usize];
            if *cell == 0 {
                continue;
            }
            sol2 = sol2.max(d);
            *cell = 0; // set this to 0 so we know we have visited it
            for new_pos in (1..=4).map(|mov| move_to(pos, mov)) {
                queue.push_back((d + 1, new_pos));
            }
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[];
}

fn _print_map(map: &[[i64; W]; H], pos: (i32, i32)) {
    let mut x_min = usize::MAX;
    let mut x_max = usize::MIN;
    let mut y_min = usize::MAX;
    let mut y_max = usize::MIN;
    for y in 0..H {
        for x in 0..W {
            if map[y][x] != -1 {
                x_min = x_min.min(x);
                x_max = x_max.max(x);
                y_min = y_min.min(y);
                y_max = y_max.max(y);
            }
        }
    }
    for y in y_min..=y_max {
        for x in x_min..=x_max {
            if x == W / 2 && y == H / 2 {
                print!("S");
                continue;
            }
            if x as i32 == pos.0 && y as i32 == pos.1 {
                print!("D");
                continue;
            }
            let c = match map[y][x] {
                -1 => ' ',
                0 => '#',
                1 => '.',
                2 => 'O',
                _ => unreachable!(),
            };
            print!("{c}");
        }
        println!();
    }
}
