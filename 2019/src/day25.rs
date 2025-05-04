use aoc_lib_rust::{Day, Solution};
use super::intcode_computer::{Comp, Interrupt};

pub struct Day25;

impl Day for Day25 {

    const PART1: Solution = Solution::U32(537002052);
    const PART2: Solution = Solution::None;

    fn solve(input: &str) -> [Solution; 2] {
        let prog = Comp::parse_prog(input);
        let mut comp = Comp::new(prog, []);

        // uncomment this to play the text adventure yourself
        /*
        loop {
            match comp.exec() {
                Interrupt::Output(out) => print!("{}", char::try_from(out as u32).unwrap()),
                Interrupt::Input => {
                    let mut input = String::new();
                    std::io::stdin().read_line(&mut input).expect("Failed to read line");
                    comp.extend_input(input.chars().map(|c| c as i64));
                }
                Interrupt::Halt => break,
            }
        }
        */

        // if any of these items is taken, the game ends
        let items_to_not_take = [
            "photons", "infinite loop", "molten lava", "escape pod", "giant electromagnet",
        ];

        // first move through the whole map up to Security Checkpoint
        // collect all items not in the list above on the way
        // do this by going always right first (this way Security will be the last room)
        let mut output = String::new();
        let mut items = Vec::<String>::new();
        let mut inv = Vec::<String>::new();
        let mut path = vec![(2, None::<Vec<i32>>)];
        'outer: loop {
            match comp.exec() {
                Interrupt::Output(out) => {
                    output.push(char::try_from(out as u32).unwrap());
                    continue;
                }
                Interrupt::Input => {
                    if let Some(item) = items.pop() {
                        comp.extend_input(b"take ".map(i64::from));
                        comp.extend_input(item.bytes().map(i64::from));
                        comp.push_input(b'\n' as i64);
                        inv.push(item);
                        continue;
                    }
                    let from = path.last_mut().unwrap();
                    if let Some(dirs) = from.1.as_mut() {
                        let dir = dirs.pop().unwrap();
                        let txt_dir = ["north", "east", "south", "west"][dir as usize];
                        comp.extend_input(txt_dir.bytes().map(i64::from));
                        comp.push_input(b'\n' as i64);
                        if dirs.is_empty() {
                            path.pop();
                        }
                        path.push(((dir + 2) % 4, None));
                        continue;
                    }
                }
                Interrupt::Halt => break,
            }

            let from = path.last_mut().unwrap();
            if from.1.is_none() {
                let mut dirs = Vec::new();
                for line in output.lines() {
                    if line.starts_with("== Security Checkpoint") {
                        break 'outer;
                    } else if line.starts_with("-") {
                        match &line[2..] {
                            "north" => dirs.push(0),
                            "east"  => dirs.push(1),
                            "south" => dirs.push(2),
                            "west"  => dirs.push(3),
                            item => {
                                if !items_to_not_take.contains(&item) {
                                    items.push(item.to_owned());
                                }
                            }
                        }
                    }
                }
                dirs.sort_unstable_by_key(|dir| (dir + 4 - from.0) % 4);
                from.1 = Some(dirs);
            }
            output.clear();
        }

        debug_assert_eq!(inv.len(), 8, "expected to collect 8 items");

        fn adv_item(comp: &mut Comp, output: &mut String, action: &str, item: &str) {
            output.clear();
            comp.extend_input(action.bytes().map(i64::from));
            comp.push_input(b' ' as i64);
            comp.extend_input(item.bytes().map(i64::from));
            comp.push_input(b'\n' as i64);
            loop {
                match comp.exec() {
                    Interrupt::Output(out) => output.push(char::try_from(out as u32).unwrap()),
                    _ => break,
                }
            }
        }

        fn adv_go(comp: &mut Comp, output: &mut String, dir: &str) {
            output.clear();
            comp.extend_input(dir.bytes().map(i64::from));
            comp.push_input(b'\n' as i64);
            loop {
                match comp.exec() {
                    Interrupt::Output(out) => output.push(char::try_from(out as u32).unwrap()),
                    _ => break,
                }
            }
        }

        // then try all combinations of items until you have the right weight to pass
        // the  security floor
        let mut sol1 = 0;
        let mut items = !0;
        for i in 1..u8::MAX {
            // use gray code generation to only change one item every attempt
            let items_next = !(i ^ (i >> 1));
            let diff = items ^ items_next;
            debug_assert_eq!(diff.count_ones(), 1);
            let item = &inv[diff.trailing_zeros() as usize];
            if items > items_next {
                adv_item(&mut comp, &mut output, "drop", item);
            } else {
                adv_item(&mut comp, &mut output, "take", item);
            }
            adv_go(&mut comp, &mut output, "north");

            for line in output.lines() {
                if line.starts_with("\"Oh,") {
                    sol1 = line["\"Oh, hello! You should be able to get in by typing ".len()..]
                        .split(" ").next().unwrap().parse().unwrap();
                    break;
                }
            }

            items = items_next;
        }

        [Solution::U32(sol1), Solution::None]
    }
}
