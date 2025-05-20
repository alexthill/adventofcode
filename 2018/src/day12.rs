use aoc_lib_rust::{Day, Example, Solution};

pub struct Day12;

impl Day for Day12 {

    const PART1: Solution = Solution::U32(2444);
    const PART2: Solution = Solution::U64(750000000697);

    fn solve(input: &str) -> [Solution; 2] {
        let mut lines = input.as_bytes().split(|c| *c == b'\n');
        let init_state = &lines.next().unwrap()[b"initial state: ".len()..];
        let mut rules = [false; 1 << 5];
        assert_eq!(lines.next(), Some(&b""[..]));
        for line in lines {
            let idx = line.iter().take(5).fold(0, |acc, c| {
                match c {
                    b'.' =>  acc << 1,
                    b'#' =>  (acc << 1) | 1,
                    _ => unreachable!(),
                }
            });
            rules[idx] = line[b"...## => ".len()] == b'#';
        }

        let mut state = vec![false; 80 + init_state.len()];
        let mut state_copy = state.clone();
        let offset = 20;
        for (i, c) in init_state.iter().enumerate() {
            state[i + offset] = *c == b'#';
        }

        for _ in 0..20 {
            let mut idx = state.iter().take(4).fold(0, |acc, x| (acc << 1) | (*x as usize));
            for i in 2..state.len() - 2 {
                idx = (0b11111 & (idx << 1)) | (state[i + 2] as usize);
                state_copy[i] = rules[idx];
            }
            std::mem::swap(&mut state, &mut state_copy);
        }

        let sol1 = state.iter().enumerate()
            .filter_map(|(i, &x)| if x { Some(i as i32 - offset as i32) } else { None })
            .sum::<i32>();

        let mut gen = 20;
        let mut shift = 0;
        loop {
            let mut idx = state.iter().take(4).fold(0, |acc, x| (acc << 1) | (*x as usize));
            for i in 2..state.len() - 2 {
                idx = (0b11111 & (idx << 1)) | (state[i + 2] as usize);
                state_copy[i] = rules[idx];
            }
            std::mem::swap(&mut state, &mut state_copy);
            if state[state.len() - 5] {
                let first = state.iter().position(|x| *x).unwrap();
                let len = state.len() - first;
                state.copy_within(first..first + len, 5);
                for i in 5 + len..state.len() {
                    state[i] = false;
                }
                shift += first - 5;
            }
            if &state[1..] == &state_copy[..state_copy.len() - 1] {
                break;
            }
            gen += 1;
        }

        let offset = 50000000000 - offset as i64 + shift as i64 - gen as i64 - 1;
        let sol2 = state.iter().enumerate()
            .filter_map(|(i, &x)| if x { Some(i as i64 + offset) } else { None })
            .sum::<i64>();

        [Solution::U32(sol1 as _), Solution::U64(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(325), Solution::U64(999999999374)],
            input: "initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #",
        },
    ];
}
