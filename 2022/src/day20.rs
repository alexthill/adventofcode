use crate::Solution;
use crate::utils::iter_parse_i32;

pub const SOLUTION: Solution = Solution::U64((4151, 7848878698663));

#[derive(Clone)]
struct Node {
    prev: u32,
    next: u32,
    value: i64,
}

pub fn solve(input: String) -> Solution {
    let mut iter = input.bytes();
    let mut zero_pos = 0;
    let mut nodes = Vec::new();
    nodes.push(Node { prev: 0, next: 1, value: iter_parse_i32(&mut iter).0.unwrap() as i64 });
    while let Some(num) = iter_parse_i32(&mut iter).0 {
        let idx = nodes.len() as u32;
        nodes.push(Node { prev: idx - 1, next: idx + 1, value: num as i64 });
        if num == 0 {
            zero_pos = idx;
        }
    }
    nodes.first_mut().unwrap().prev = nodes.len() as u32 - 1;
    nodes.last_mut().unwrap().next = 0;
    
    let mut nodes2 = nodes.clone();
    for node in nodes2.iter_mut() {
        node.value *= 811589153;
    }
    
    mix_list(&mut nodes);
    let res1 = (1..=3).map(|n| list_nth(&nodes, zero_pos, n * 1000)).sum::<i64>();
    
    for _ in 0..10 {
        mix_list(&mut nodes2);
    }
    let res2 = (1..=3).map(|n| list_nth(&nodes2, zero_pos, n * 1000)).sum::<i64>();
    
    (res1 as u64, res2 as u64).into()
}

fn mix_list(nodes: &mut [Node]) {
    let len = nodes.len() as i64 - 1;
    for i in 0..nodes.len() {
        let num = (nodes[i].value % len + len) % len;
        if num > len / 2 {
            let mut next = nodes[i].next;
            let mut prev = nodes[i].prev;
            nodes[next as usize].prev = nodes[i].prev;
            nodes[prev as usize].next = nodes[i].next;
            for _ in 0..len - num {
                prev = nodes[prev as usize].prev;
            }
            next = nodes[prev as usize].next;
            nodes[next as usize].prev = i as u32;
            nodes[prev as usize].next = i as u32;
            nodes[i].next = next;
            nodes[i].prev = prev;
        } else if num != 0 {
            let mut next = nodes[i].next;
            let mut prev = nodes[i].prev;
            nodes[next as usize].prev = nodes[i].prev;
            nodes[prev as usize].next = nodes[i].next;
            for _ in 0..num {
                next = nodes[next as usize].next;
            }
            prev = nodes[next as usize].prev;
            nodes[next as usize].prev = i as u32;
            nodes[prev as usize].next = i as u32;
            nodes[i].next = next;
            nodes[i].prev = prev;
        }
    }
}

fn list_nth(nodes: &[Node], start: u32, mut n: usize) -> i64 {
    n = n % nodes.len();
    let mut idx = start;
    loop {
        if n == 0 {
            return nodes[idx as usize].value;
        }
        idx = nodes[idx as usize].next;
        n -= 1;
    }
}
