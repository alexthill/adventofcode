use std::collections::VecDeque;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Interrupt {
    Output(i64),
    Input,
    Halt,
}

#[derive(Default)]
pub struct Comp {
    prog: Vec<i64>,
    input: VecDeque<i64>,
    ic: usize,
    halted: bool,
    output: Option<i64>,
    relative_offset: i64,
}

impl Comp {
    pub fn new(prog: Vec<i64>, input: impl Into<VecDeque<i64>>) -> Self {
        Self {
            prog,
            input: input.into(),
            ..Default::default()
        }
    }

    pub fn parse_prog(input: &str) -> Vec<i64> {
        input.split(',')
            .map(|s| s.parse::<i64>().unwrap())
            .collect::<Vec<_>>()
    }

    pub fn halted(&self) -> bool {
        self.halted
    }

    pub fn output(&self) -> Option<i64> {
        self.output
    }

    pub fn push_input(&mut self, input: i64) {
        self.input.push_back(input);
    }

    pub fn extend_input<T, I>(&mut self, input: T)
    where
        T: IntoIterator<Item = I>,
        I: Into<i64>,
    {
        self.input.extend(input.into_iter().map(|x| x.into()));
    }

    pub fn exec_one(&mut self) -> Option<Interrupt> {
        if self.halted {
            return Some(Interrupt::Halt);
        }
        self.ic += match self.prog[self.ic] % 100 {
            1 => self.write(3, self.args([1, 2]).iter().sum()),
            2 => self.write(3, self.args([1, 2]).iter().product()),
            3 => {
                let Some(value) = self.input.pop_front() else {
                    return Some(Interrupt::Input);
                };
                self.write(1, value)
            }
            4 => {
                let addr = self.get_addr(self.prog[self.ic] / 100, 1);
                self.output = Some(self.prog[addr]);
                self.ic += 2;
                return Some(Interrupt::Output(self.prog[addr]));
            }
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
            9 => {
                self.relative_offset += self.args([1])[0];
                2
            }
            99 => {
                self.halted = true;
                return Some(Interrupt::Halt);
            }
            _ => unreachable!(),
        };
        None
    }

    pub fn exec(&mut self) -> Interrupt {
        loop {
            if let Some(interrupt) = self.exec_one() {
                return interrupt;
            }
        }
    }

    pub fn reset(&mut self, prog: &[i64]) {
        self.prog.truncate(prog.len()); // prog might got bigger during execution
        self.prog.copy_from_slice(prog);
        self.input.clear();
        self.ic = 0;
        self.halted = false;
        self.output = None;
        self.relative_offset = 0;
    }

    fn write(&mut self, idx: usize, value: i64) -> usize {
        let mode = match idx {
            1 => self.prog[self.ic] / 100,
            3 => self.prog[self.ic] / 10000,
            _ => unreachable!(),
        };
        let addr = self.get_addr(mode, idx);
        if addr >= self.prog.len() {
            self.prog.resize(addr + 1, 0);
        }
        self.prog[addr] = value;
        idx + 1
    }

    fn args<const N: usize>(&self, args: [usize; N]) -> [i64; N] {
        let mut op = self.prog[self.ic] / 100;
        args.map(|idx| {
            let addr = self.get_addr(op % 10, idx);
            op /= 10;
            self.prog.get(addr).copied().unwrap_or(0)
        })
    }

    fn get_addr(&self, mode: i64, idx: usize) -> usize {
        match mode {
            0 => self.prog[self.ic + idx] as usize,
            1 => self.ic + idx,
            2 => (self.prog[self.ic + idx] + self.relative_offset) as usize,
            _ => unreachable!(),
        }
    }
}
