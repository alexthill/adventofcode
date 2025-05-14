use aoc_lib_rust::{Day, Example, Solution};

#[derive(Debug)]
struct Node {
    child_count: u32,
    child_values: Vec<u32>,
    meta_count: u32,
    value: u32,
}

pub struct Day08;

impl Day for Day08 {

    const PART1: Solution = Solution::U32(37262);
    const PART2: Solution = Solution::U32(20839);

    fn solve(input: &str) -> [Solution; 2] {
        let mut iter = input.split(' ').map(|x| x.parse::<u32>().unwrap());
        let mut queue: Vec<Node> = Vec::new();
        let mut sol1 = 0;
        let head = loop {
            if let Some(mut node) = queue.pop() {
                if node.child_count as usize == node.child_values.len() {
                    let mut meta_sum = 0;
                    for _ in 0..node.meta_count {
                        let meta = iter.next().unwrap();
                        meta_sum += meta;
                        node.value += node.child_values.get(meta as usize - 1)
                            .copied().unwrap_or(0);
                    }
                    sol1 += meta_sum;
                    if let Some(parent) = queue.last_mut() {
                        if node.child_count == 0 {
                            node.value = meta_sum;
                        }
                        parent.child_values.push(node.value);
                    } else {
                        break node;
                    }
                    continue;
                }
                queue.push(node);
            }
            queue.push(Node {
                child_count: iter.next().unwrap(),
                child_values: Vec::new(),
                meta_count: iter.next().unwrap(),
                value: 0,
            });
        };

        [Solution::U32(sol1), Solution::U32(head.value)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(138), Solution::U32(66)],
            input: "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2",
        },
    ];
}
