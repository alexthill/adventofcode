use crate::Solution;

pub const SOLUTION: Solution = Solution::Str(("2-0=11=-0-2-1==1=-22", ""));

pub fn solve(input: String) -> Solution {
    let mut sum: i64 = 0;
    for num_in in input.as_bytes().split(|&c| c== b'\n') {
        let mut num = 0;
        let mut mul = 1;
        for d in num_in.iter().rev() {
            match d {
                b'0' => {}
                b'1' => num += mul,
                b'2' => num += 2 * mul,
                b'-' => num += -1 * mul,
                b'=' => num += -2 * mul,
                other => panic!("unexpected char {}", other),
            }
            mul *= 5;
        }
        sum += num;
    }
    
    let mut base5_num = Vec::new();
    while sum != 0 {
        base5_num.push((sum % 5) as u8 + b'0');
        sum /= 5;
    }
    
    for i in 0..base5_num.len() {
        if base5_num[i] > b'2' {
            if i + 1 < base5_num.len() {
                base5_num[i + 1] += 1;
            } else {
                base5_num.push(b'1');
            }
            match &mut base5_num[i] {
                d @ b'3' => *d = b'=',
                d @ b'4' => *d = b'-',
                d @ b'5' => *d = b'0',
                _ => {}
            }
        }
    }
    base5_num.reverse();
    
    (String::from_utf8(base5_num).unwrap(), "".to_string()).into()
}
