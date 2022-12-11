use crate::Solution;
use crate::utils::iter_parse_u32;

pub const SOLUTION: Solution = Solution::U64((57838, 15050382231));

#[derive(Debug, Clone)]
enum Op {
    Add(u32),
    Mul(u32),
    MulOld,
}

#[derive(Debug, Clone)]
struct Monkey {
    items: Vec<u32>,
    op: Op,
    test: u32,
    if_true: u8,
    if_false: u8,
    count: u32,
}

pub fn solve(input: String) -> Solution {
    let mut monkeys = Vec::new();
    let mut test_product = 1;
    
    let mut iter = input.into_bytes().into_iter();
    while iter.nth(b"Monkey 0:\n  Starting items:".len()).is_some() {
        let mut items = Vec::new();
        while let (Some(num), _) = iter_parse_u32(&mut iter) {
            items.push(num);
            iter.nth(0).unwrap();
        }
        
        iter.nth(b"Operation: new = old".len()).unwrap();
        let symbol = iter.next().unwrap();
        iter.nth(0).unwrap();
        let op = if let Some(num) = iter_parse_u32(&mut iter).0 {
            match symbol {
                b'+' => Op::Add(num),
                b'*' => Op::Mul(num),
                other => panic!("unknown operation {}", other),
            }
        } else {
            iter.nth(b"ld".len()).unwrap();
            Op::MulOld
        };
        
        iter.nth(b"  Test: divisible by".len()).unwrap();
        let test = iter_parse_u32(&mut iter).0.unwrap();
        
        iter.nth(b"    If true: throw to monkey".len()).unwrap();
        let if_true = iter_parse_u32(&mut iter).0.unwrap() as u8;
        
        iter.nth(b"    If false: throw to monkey".len()).unwrap();
        let if_false = iter_parse_u32(&mut iter).0.unwrap() as u8;
        
        monkeys.push(Monkey { items, op, test, if_true, if_false, count: 0 });
        test_product *= test;
        iter.nth(0);
    }
    
    let monkeys2 = monkeys.clone();
    
    for _ in 0..20 {
        for m in 0..monkeys.len() {
            monkeys[m].count += monkeys[m].items.len() as u32;
            
            for i in 0..monkeys[m].items.len() {
                let old_val = monkeys[m].items[i];
                let new_val = match monkeys[m].op {
                    Op::Add(num) => old_val + num,
                    Op::Mul(num) => old_val * num,
                    Op::MulOld => old_val.pow(2),
                } / 3;
                
                let to = if new_val % monkeys[m].test == 0 {
                    monkeys[m].if_true
                } else {
                    monkeys[m].if_false
                };
                monkeys[to as usize].items.push(new_val);
            }
            
            monkeys[m].items.clear();
        }
    }
    
    let max = find_two_max_counts(&monkeys);
    let business = max.0 as u64 * max.1 as u64;
    let mut monkeys = monkeys2;
    
    for _ in 0..10000 {
        for m in 0..monkeys.len() {
            monkeys[m].count += monkeys[m].items.len() as u32;
            
            for i in 0..monkeys[m].items.len() {
                let old_val = monkeys[m].items[i];
                let new_val = match monkeys[m].op {
                    Op::Add(num) => old_val + num,
                    Op::Mul(num) => (old_val * num) % test_product,
                    Op::MulOld => ((old_val as u64).pow(2) % test_product as u64) as u32,
                };
                
                let to = if new_val % monkeys[m].test == 0 {
                    monkeys[m].if_true
                } else {
                    monkeys[m].if_false
                };
                monkeys[to as usize].items.push(new_val);
            }
            
            monkeys[m].items.clear();
        }
    }
    
    let max = find_two_max_counts(&monkeys);
    let business2 = max.0 as u64 * max.1 as u64;
    
    (business, business2).into()
}

fn find_two_max_counts(monkeys: &[Monkey]) -> (u32, u32) {
    let mut max = (0, 0);
    
    for monkey in monkeys.iter() {
        let count = monkey.count;
        if count > max.0 {
            max.1 = max.0;
            max.0 = count;
        } else if count > max.1 {
            max.1 = count;
        }
    }
    
    max
}
