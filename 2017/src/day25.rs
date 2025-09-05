use aoc_lib_rust::{next, next_parse, Day, Example, Solution};

#[derive(Debug, Default, Clone, Copy)]
struct Instruction {
    write: u8,
    move_to: i8,
    next_state: u8,
}

pub struct Day25;

impl Day for Day25 {

    const PART1: Solution = Solution::U32(3732);
    const PART2: Solution = Solution::None;

    fn solve(input: &str) -> [Solution; 2] {
        let mut lines = input.lines();
        let start_state = next!(lines).as_bytes()["Begin in state ".len()] - b'A';
        let steps = next_parse!(next!(lines).split(' ').skip(5), u32);

        let mut states = [[Instruction::default(); 2]; 8];
        while let Some(line) = lines.next() {
            assert!(line.is_empty(), "line is '{line}'");
            let state_idx = next!(lines).as_bytes()["In state ".len()] - b'A';
            let state = &mut states[state_idx as usize];
            for value in 0..=1 {
                assert_eq!(next!(lines), format!("  If the current value is {value}:").as_str());
                let line = next!(lines);
                state[value].write =
                    line["    - Write the value ".len()..line.len() - 1].parse().unwrap();
                state[value].move_to = match next!(lines) {
                    "    - Move one slot to the right." => 1,
                    "    - Move one slot to the left." => -1,
                    _ => unreachable!(),
                };
                state[value].next_state
                    = next!(lines).as_bytes()["    - Continue with state ".len()] - b'A';
            }
        }

        let mut tape = Vec::new();
        let mut curr_state = start_state;
        let mut curr_pos: i32 = 0;
        for _ in 0..steps {
            let tape_idx = if curr_pos >= 0 { curr_pos * 2 } else { -curr_pos * 2 - 1 } as usize;
            if tape_idx >= tape.len() {
                tape.resize(tape_idx + 1, 0);
            }
            let instruction = states[curr_state as usize][tape[tape_idx] as usize];
            tape[tape_idx] = instruction.write;
            curr_pos += instruction.move_to as i32;
            curr_state = instruction.next_state;
        }
        let sol1 = tape.into_iter().filter(|value| *value == 1).count();

        [Solution::U32(sol1 as _), Solution::None]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(3), Solution::None],
            input: "Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.",
        },
    ];
}
