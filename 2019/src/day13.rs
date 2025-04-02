use super::intcode_computer::{Comp, Interrupt};
use aoc_lib_rust::{Day, Example, Solution};

pub struct Day13;

impl Day for Day13 {

    const PART1: Solution = Solution::U32(180);
    const PART2: Solution = Solution::U32(8777);

    fn solve(input: &str) -> [Solution; 2] {
        const W: usize = 36;
        const H: usize = 21;
        let mut prog = Comp::parse_prog(input);
        let mut display = [[0; W]; H];

        let mut comp = Comp::new(prog.clone(), []);
        while !comp.halted() {
            comp.exec();
            let x = comp.output().unwrap();
            if comp.halted() { break; }
            comp.exec();
            let y = comp.output().unwrap();
            if comp.halted() { break; }
            comp.exec();
            let tile_id = comp.output().unwrap();
            display[y as usize][x as usize] = tile_id;
        }
        let sol1 = display.iter().map(|row| row.iter().filter(|c| **c == 2).count() as u32).sum();

        fn _print_display(display: &[[i64; W]; H]) {
            for row in display.iter() {
                let s = row.iter().map(|c| {
                    match c {
                        0 => '.',
                        1 => 'W',
                        2 => 'B',
                        3 => '_',
                        4 => 'o',
                        _ => unreachable!(),
                    }
                }).collect::<String>();
                println!("{s}");
            }
        }

        fn get_input(_display: &[[i64; W]; H], ball: [i64; 2], paddle: [i64; 2]) -> i64 {
            // _print_display(_display);
            // std::thread::sleep(std::time::Duration::from_millis(33));
            match ball[0].cmp(&paddle[0]) {
                std::cmp::Ordering::Greater => 1,
                std::cmp::Ordering::Less => -1,
                std::cmp::Ordering::Equal => 0,
            }
        }

        prog[0] = 2;
        let mut comp = Comp::new(prog, []);
        let mut sol2 = 0;
        let mut ball = [0; 2];
        let mut paddle = [0; 2];
        loop {
            let [x, y, tile_id] = [0; 3].map(|_| {
                loop {
                    match comp.exec() {
                        Interrupt::Output(n) => return n,
                        Interrupt::Input => comp.push_input(get_input(&display, ball, paddle)),
                        Interrupt::Halt => return -42,
                    }
                }
            });
            if comp.halted() {
                break;
            }
            if (x, y) == (-1, 0) {
                sol2 = tile_id;
            } else {
                display[y as usize][x as usize] = tile_id;
                match tile_id {
                    3 => paddle = [x, y],
                    4 => ball = [x, y],
                    _ => {}
                }
            }
        }

        [Solution::U32(sol1), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[];
}
