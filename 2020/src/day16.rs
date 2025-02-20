use aoc_lib_rust::{Day, Example, Solution};
use std::ops::Range;

struct Field {
    name: String,
    range1: Range<u32>,
    range2: Range<u32>,
    valid_idx: u32,
}

pub struct Day16;

impl Day for Day16 {

    const PART1: Solution = Solution::U32(32842);
    const PART2: Solution = Solution::U64(2628667251989);

    fn solve(input: &str) -> [Solution; 2] {
        let mut lines = input.lines();
        let mut fields = Vec::new();

        while let Some(line) = lines.next() {
            if line.is_empty() {
                break;
            }
            let (name, line) = line.split_once(|c| c == ':').unwrap();
            let parts = line.split([' ', '-']).collect::<Vec<_>>();
            let range1 = Range {
                start: parts[1].parse::<u32>().unwrap(),
                end: parts[2].parse::<u32>().unwrap() + 1,
            };
            let range2 = Range {
                start: parts[4].parse::<u32>().unwrap(),
                end: parts[5].parse::<u32>().unwrap() + 1,
            };
            fields.push(Field {
                name: name.to_owned(),
                range1,
                range2,
                valid_idx: !0,
            });
        }

        // "your ticket"
        lines.next().unwrap();
        let my_ticket = lines.next().unwrap();
        let my_nums = my_ticket.split(',')
            .map(|part| part.parse::<u32>().unwrap())
            .collect::<Vec<_>>();
        for field in &mut fields {
            field.valid_idx &= (1 << my_nums.len()) - 1;
        }
        // empty line
        lines.next().unwrap();
        // "nearby tickets"
        lines.next().unwrap();

        let mut sol1 = 0;
        for line in lines {
            let nums = line.split(',')
                .map(|part| part.parse::<u32>().unwrap())
                .collect::<Vec<_>>();

            if let Some(bad_num) = nums.iter().find(|num| {
                fields.iter().all(|field| {
                    !field.range1.contains(num) && !field.range2.contains(num)
                })
            }) {
                sol1 += *bad_num;
                continue;
            };

            for (i, num) in nums.iter().enumerate() {
                for field in &mut fields {
                    if !field.range1.contains(num) && !field.range2.contains(num) {
                        field.valid_idx &= !(1 << i);
                    }
                }
            }
        }

        fn update_fields(mask: u32, fields: &mut [Field]) {
            for i in 0..fields.len() {
                if fields[i].valid_idx.count_ones() != 1 {
                    fields[i].valid_idx &= mask;
                    if fields[i].valid_idx.count_ones() == 1 {
                        update_fields(!fields[i].valid_idx, fields);
                    }
                }
            }
        }
        for i in 0..fields.len() {
            if fields[i].valid_idx.count_ones() == 1 {
                update_fields(!fields[i].valid_idx, &mut fields);
            }
        }

        let sol2 = fields.into_iter()
            .filter(|field| field.name.starts_with("departure"))
            .map(|field| my_nums[field.valid_idx.trailing_zeros() as usize] as u64)
            .product();

        [Solution::U32(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(71), Solution::U32(14)],
            input: "class: 1-3 or 5-7
row: 6-11 or 33-44
departure: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12",
        },
    ];
}
