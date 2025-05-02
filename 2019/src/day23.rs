use aoc_lib_rust::{Day, Solution};
use super::intcode_computer::{Comp, Interrupt};

enum Packet {
    Empty,
    One(i64),
    Two(i64, i64),
    Three(i64, i64, i64),
}

impl Packet {
    fn next(&mut self, n: i64) {
        match self {
            Self::Empty => *self = Self::One(n),
            Self::One(a) => *self = Self::Two(*a, n),
            Self::Two(a, b) => *self = Self::Three(*a, *b, n),
            _ => panic!("cannot add to finished packet"),
        }
    }

    fn done(&mut self) -> Option<(i64, i64, i64)> {
        if let Self::Three(a, b, c) = self {
            let res = (*a, *b, *c);
            *self = Self::Empty;
            Some(res)
        } else {
            None
        }
    }
}

impl Default for Packet {
    fn default() -> Self { Self::Empty }
}

pub struct Day23;

impl Day for Day23 {

    const PART1: Solution = Solution::U32(20367);
    const PART2: Solution = Solution::U32(15080);

    fn solve(input: &str) -> [Solution; 2] {
        let prog = Comp::parse_prog(input);

        let mut comps: [_; 50] = std::array::from_fn(|i| {
            (Comp::new(prog.clone(), [i as i64]), Packet::Empty, 0)
        });

        let mut nat = None;
        let mut last_nat_y = 0;
        let mut sol1 = 0;
        let sol2 = 'outer: loop {
            let mut waiting_count = 0;
            for (comp, packet, waiting) in &mut comps {
                if *waiting > 1 {
                    waiting_count += 1;
                    continue;
                }
                match comp.exec() {
                    Interrupt::Output(out) => {
                        packet.next(out);
                    }
                    Interrupt::Input => {
                        comp.push_input(-1);
                        *waiting += 1;
                    }
                    _ => {}
                }
            }
            if waiting_count == 50 {
                let [x, y] = nat.unwrap();
                if y == last_nat_y {
                    break 'outer y;
                }
                comps[0].0.extend_input([x, y]);
                comps[0].2 = 0;
                last_nat_y = y;
            } else {
                for i in 0..50 {
                    if let Some((addr, x, y)) = comps[i].1.done() {
                        if addr == 255 {
                            if nat.is_none() {
                                sol1 = y;
                            }
                            nat = Some([x, y]);
                        } else {
                            comps[addr as usize].0.extend_input([x, y]);
                            comps[addr as usize].2 = 0;
                        }
                    }
                }
            }
        };

        [Solution::U64(sol1 as _), Solution::U64(sol2 as _)]
    }
}
