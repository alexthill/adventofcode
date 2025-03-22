use aoc_lib_rust::{Day, Example, Solution};

pub struct Comp {
    prog: Vec<i64>,
    ic: usize,
}

impl Comp {
    fn new(prog: Vec<i64>) -> Self {
        Self { prog, ic: 0 }
    }

    fn write(&mut self, idx: usize, value: i64) -> usize {
        let idx2 = self.prog[self.ic + idx] as usize;
        self.prog[idx2] = value;
        idx + 1
    }

    fn args<const N: usize>(&self, args: [usize; N]) -> [i64; N] {
        let mut op = self.prog[self.ic] / 100;
        args.map(|idx| {
            let arg = match op % 10 {
                0 => self.prog[self.prog[self.ic + idx] as usize],
                1 => self.prog[self.ic + idx],
                _ => unreachable!(),
            };
            op /= 10;
            arg
        })
    }

    fn exec(&mut self, input: i64) -> i64 {
        let mut output = 0;
        loop {
            self.ic += match self.prog[self.ic] % 100 {
                1 => self.write(3, self.args([1, 2]).iter().sum()),
                2 => self.write(3, self.args([1, 2]).iter().product()),
                3 => self.write(1, input),
                4 => { output = self.prog[self.prog[self.ic + 1] as usize]; 2 }
                5 => match self.args([1, 2]) {
                    [0, _] => 3,
                    [_, a] => { self.ic = a as _; 0 }
                }
                6 => match self.args([1, 2]) {
                    [0, a] => { self.ic = a as _; 0 }
                    [_, _] => 3,
                }
                7 => self.write(3, self.args([1, 2]).windows(2).all(|a| a[0] < a[1]) as _),
                8 => self.write(3, self.args([1, 2]).windows(2).all(|a| a[0] == a[1]) as _),
                99 => break output,
                _ => unreachable!(),
            };
        }
    }
}

pub struct Day05;

impl Day for Day05 {

    const PART1: Solution = Solution::U64(16209841);
    const PART2: Solution = Solution::U64(8834787);

    fn solve(input: &str) -> [Solution; 2] {
        let prog = input.split(',')
            .map(|s| s.parse::<i64>().unwrap())
            .collect::<Vec<_>>();

        let sol1 = Comp::new(prog.clone()).exec(1) as _;
        let sol2 = Comp::new(prog).exec(5) as _;

        [Solution::U64(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[];
}
