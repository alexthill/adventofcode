use std::fs;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
enum Number {
    Regular(u32),
    Pair(Box<(Number, Number)>),
}
impl Number {
    fn new_pair(left: Number, right: Number) -> Self {
        Number::Pair(Box::new((left, right)))
    }
    
    fn from_str(string: &str) -> Self {
        let mut stack = vec![];
        for ch in string.chars() {
            match ch {
                '[' | ',' => {},
                ch @ '0'..='9' => stack.push(Number::Regular(ch.to_digit(10).unwrap())),
                ']' => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(Number::new_pair(left, right));
                },
                other => panic!("unexpected char {:#?}", other),
            }
        }
        assert_eq!(stack.len(), 1);
        stack.pop().unwrap()
    }
    
    fn add(self, other: Number) -> Self {
        let mut sum = Number::new_pair(self, other);
        sum.reduce();
        sum
    }
    
    fn reduce(&mut self) {
        loop {
            if self.explode(0).is_some() {
                continue;
            }
            if self.split() {
                continue;
            }
            break;
        }
    }
    
    fn split(&mut self) -> bool {
        match self {
            Number::Regular(reg) => {
                if *reg < 10 {
                    false
                } else {
                    let left = Number::Regular(*reg / 2);
                    let right = Number::Regular(*reg / 2 + if *reg % 2 == 0 { 0 } else { 1 });
                    *self = Number::new_pair(left, right);
                    true
                }
            }
            Number::Pair(pair) => {
                pair.0.split() || pair.1.split()
            }
        }
    }
    
    fn explode(&mut self, depth: u32) -> Option<(Option<u32>, Option<u32>)> {
        if let Number::Pair(pair) = self {
            if depth >= 4 {
                if let Number::Regular(l) = pair.0 {
                    if let Number::Regular(r) = pair.1 {
                        return Some((Some(l), Some(r)));
                    }
                }
            }
            
            if let Some(res) = pair.0.explode(depth + 1) {
                return Some(match res {
                    (Some(l), Some(r)) => {
                        pair.0 = Number::Regular(0);
                        pair.1.add_explode_l(r);
                        (Some(l), None)
                    }
                    (None, Some(r)) => {
                        pair.1.add_explode_l(r);
                        (None, None)
                    }
                    other => other,
                });
            }
            
            if let Some(res) = pair.1.explode(depth + 1) {
                return Some(match res {
                    (Some(l), Some(r)) => {
                        pair.1 = Number::Regular(0);
                        pair.0.add_explode_r(l);
                        (None, Some(r))
                    }
                    (Some(l), None) => {
                        pair.0.add_explode_r(l);
                        (None, None)
                    }
                    other => other,
                });
            }
        }
        
        None
    }
    
    fn add_explode_l(&mut self, num: u32) {
        match self {
            Number::Regular(reg) => *reg += num,
            Number::Pair(pair) => pair.0.add_explode_l(num),
        }
    }
    
    fn add_explode_r(&mut self, num: u32) {
        match self {
            Number::Regular(reg) => *reg += num,
            Number::Pair(pair) => pair.1.add_explode_r(num),
        }
    }
    
    fn magnitude(&self) -> u32 {
        match self {
            Number::Regular(reg) => *reg,
            Number::Pair(pair) => 3 * pair.0.magnitude() + 2 * pair.1.magnitude(),
        }
    }
}
impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Number::Regular(reg) => write!(f, "{}", reg),
            Number::Pair(pair) => write!(f, "[{},{}]", pair.0, pair.1),
        }
    }
}

pub fn solve() {
    let input = fs::read_to_string("inputs/18.txt").unwrap();
    let mut lines = input.lines();
    let mut nums = vec![];
    let mut sum = Number::from_str(lines.next().unwrap());
    nums.push(sum.clone());
    for line in lines {
        let num = Number::from_str(line);
        nums.push(num.clone());
        sum = sum.add(num);
    }
    
    println!("magnitude: {}", sum.magnitude());
    
    let mut max_mag = 0;
    for num1 in nums.iter() {
        for num2 in nums.iter() {
            let sum_mag = num1.clone().add(num2.clone()).magnitude();
            if sum_mag > max_mag {
                max_mag = sum_mag;
            }
        }
    }
    
    println!("max magnitude: {}", max_mag);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_explode() {
        let mut num = Number::from_str("[[[[[9,8],1],2],3],4]");
        num.explode(0);
        assert_eq!(num, Number::from_str("[[[[0,9],2],3],4]"));
        
        let mut num = Number::from_str("[7,[6,[5,[4,[3,2]]]]]");
        num.explode(0);
        assert_eq!(num, Number::from_str("[7,[6,[5,[7,0]]]]"));
        
        let mut num = Number::from_str("[[6,[5,[4,[3,2]]]],1]");
        num.explode(0);
        assert_eq!(num, Number::from_str("[[6,[5,[7,0]]],3]"));
        
        let mut num = Number::from_str("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]");
        num.explode(0);
        assert_eq!(num, Number::from_str("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"));
        
        let mut num = Number::from_str("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]");
        num.explode(0);
        assert_eq!(num, Number::from_str("[[3,[2,[8,0]]],[9,[5,[7,0]]]]"));
    }
    
    #[test]
    fn test_add() {
        let num1 = Number::from_str("[[[[4,3],4],4],[7,[[8,4],9]]]");
        let num2 = Number::from_str("[1,1]");
        let res = Number::from_str("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]");
        assert_eq!(num1.add(num2), res);
    }
    
    #[test]
    fn test_mag() {
        assert_eq!(Number::from_str("[[1,2],[[3,4],5]]").magnitude(), 143);
        assert_eq!(Number::from_str("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]").magnitude(), 1384);
    }
}
