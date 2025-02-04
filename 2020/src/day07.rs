use aoc_lib_rust::{Day, Example, Solution};
use std::collections::{VecDeque, HashMap, HashSet};

type Bag<'a> = [&'a[u8]; 2];

pub struct Day07;

impl Day for Day07 {

    const PART1: Solution = Solution::U32(226);
    const PART2: Solution = Solution::U32(9569);

    fn solve(input: &str) -> [Solution; 2] {
        let bag_to_find = [&b"shiny"[..], &b"gold"[..]];
        let mut contained_by = HashMap::new();
        let mut bag_contains = HashMap::new();

        for line in input.as_bytes().split(|c| *c == b'\n') {
            let mut words = line.split(|c| *c == b' ');
            let container = [words.next().unwrap(), words.next().unwrap()];
            words.next().unwrap(); // "bags"
            words.next().unwrap(); // "contain"

            let mut contains = Vec::new();
            loop {
                let num = match words.next() {
                    Some(b"no") | None => break,
                    Some(num) => num.iter().fold(0, |acc, c| acc * 10 + (*c - b'0') as u32),
                };
                let bag = [words.next().unwrap(), words.next().unwrap()];
                contained_by.entry(bag).or_insert(Vec::new()).push(container);
                contains.push((num, bag));

                let _end = words.next().unwrap(); // "bags?(,|.)"
            }
            bag_contains.insert(container, contains);
        }

        let mut can_contain = HashSet::<Bag>::new();
        let mut queue = VecDeque::new();
        queue.extend(contained_by.get(&bag_to_find).unwrap());
        while let Some(bag) = queue.pop_front() {
            if can_contain.replace(bag).is_some() {
                continue;
            }
            let Some(containers) = contained_by.get(&bag) else {
                continue;
            };
            queue.extend(containers);
        }
        let sol1 = can_contain.len() as u32;

        let mut sol2 = 0;
        let mut queue = VecDeque::new();
        queue.extend(bag_contains.get(&bag_to_find).unwrap());
        while let Some((num, bag)) = queue.pop_front() {
            sol2 += num;
            let Some(new_bags) = bag_contains.get(&bag) else {
                continue;
            };
            queue.extend(new_bags.iter().map(|(new_num, bag)| ((num * new_num), *bag)));
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(4), Solution::U32(32)],
            input: "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.",
        },
    ];
}
