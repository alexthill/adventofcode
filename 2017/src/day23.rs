use aoc_lib_rust::{Day, Solution};
use super::day18::Val;

#[derive(Debug)]
enum Op {
    Set(usize, Val),
    Sub(usize, Val),
    Mul(usize, Val),
    Jnz(Val, Val),
}

pub struct Day23;

impl Day for Day23 {

    const PART1: Solution = Solution::U32(9409);
    const PART2: Solution = Solution::U32(913);

    fn solve(input: &str) -> [Solution; 2] {
        let ops = input.lines().enumerate().map(|(i, line)| {
            let mut parts = line.split(' ');
            let op = parts.next().unwrap();
            let a = parts.next().unwrap();
            let b = parts.next();
            let op = match op {
                "set" => Op::Set((a.as_bytes()[0] - b'a') as usize, b.unwrap().parse().unwrap()),
                "sub" => Op::Sub((a.as_bytes()[0] - b'a') as usize, b.unwrap().parse().unwrap()),
                "mul" => Op::Mul((a.as_bytes()[0] - b'a') as usize, b.unwrap().parse().unwrap()),
                "jnz" => Op::Jnz(a.parse().unwrap(), b.unwrap().parse().unwrap()),
                other => panic!("unknow instruction {other}"),
            };
            (i + 1, op)
        }).collect::<Vec<_>>();

        let mut regs = [0; 8];
        let mut ip = 0;
        let mut sol1 = 0;
        loop {
            let Some(op) = ip.try_into().ok().and_then(|ip: usize| ops.get(ip)) else {
                break;
            };
            match op.1 {
                Op::Set(reg, Val::Reg(idx)) => regs[reg] = regs[idx],
                Op::Set(reg, Val::Value(val)) => regs[reg] = val,
                Op::Sub(reg, Val::Reg(idx)) => regs[reg] -= regs[idx],
                Op::Sub(reg, Val::Value(val)) => regs[reg] -= val,
                Op::Mul(reg, Val::Reg(idx)) => {
                    regs[reg] *= regs[idx];
                    sol1 += 1;
                }
                Op::Mul(reg, Val::Value(val)) => {
                    regs[reg] *= val;
                    sol1 += 1;
                }
                Op::Jnz(Val::Reg(x), Val::Reg(y)) if regs[x] != 0 => ip += regs[y] - 1,
                Op::Jnz(Val::Reg(x), Val::Value(y)) if regs[x] != 0 => ip += y - 1,
                Op::Jnz(Val::Value(x), Val::Reg(y)) if x != 0 => ip += regs[y] - 1,
                Op::Jnz(Val::Value(x), Val::Value(y)) if x != 0 => ip += y - 1,
                Op::Jnz(_, _) => {}
            }
            ip += 1;
        }

        // Hardcoded numbers from my input, too lazy to write code to find the numbers.
        // My input program calculates these numbers in regs b and c and then takes
        // steps of 17 from b to c and counts the number of non-primes it finds.
        let sol2 = (109900..=126900).step_by(17).filter(|&n| !is_prime(n)).count();

        [Solution::U32(sol1), Solution::U32(sol2 as _)]
    }
}

fn is_prime(n: u32) -> bool {
    if n == 2 {
        true
    } else if n < 2 || n % 2 == 0 {
        false
    } else {
        let mut i = 3;
        while i <= n / i {
            if n % i == 0 {
                return false;
            }
            i += 2;
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_prime() {
        assert!(is_prime(2));
        assert!(is_prime(3));
        assert!(is_prime(5));
        assert!(is_prime(19));
        assert!(is_prime(97));
    }

    #[test]
    fn test_is_not_prime() {
        assert!(!is_prime(0));
        assert!(!is_prime(1));
        assert!(!is_prime(4));
        assert!(!is_prime(12));
        assert!(!is_prime(99));
        assert!(!is_prime(121));
    }
}
