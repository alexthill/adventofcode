use super::Solution;

pub type ExampleError = (&'static Example, [Solution; 2]);

pub struct Example {
    pub input: &'static str,
    pub solution: [Solution; 2],
}

pub trait Day {

    const PART1: Solution = Solution::None;
    const PART2: Solution = Solution::None;
    const EXAMPLES: &'static [Example] = &[];
    const DAY_NONE: bool = false;

    fn solve2(input: &str, _is_example: bool) -> [Solution; 2] {
        Self::solve(input)
    }

    fn solve(_input: &str) -> [Solution; 2] {
        [Solution::None, Solution::None]
    }

    fn check_examples() -> Result<(), ExampleError> {
        for example in Self::EXAMPLES.iter() {
            let mut sol = Self::solve2(example.input, true);
            for (expected, found) in example.solution.iter().zip(sol.iter_mut()) {
                if *expected == Solution::None {
                    *found = Solution::None;
                }
            }
            if sol != example.solution {
                return Err((example, sol));
            }
        }
        Ok(())
    }
}

pub struct DayNone;

impl Day for DayNone {
    const DAY_NONE: bool = true;
}
