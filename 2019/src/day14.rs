use aoc_lib_rust::{Day, Example, Solution};
use std::collections::{HashMap, VecDeque};

const ORE: u64 = 1000000000000;

pub struct Day14;

impl Day for Day14 {

    const PART1: Solution = Solution::U32(598038);
    const PART2: Solution = Solution::U32(2269325);

    fn solve(input: &str) -> [Solution; 2] {
        let mut map = HashMap::new();
        let mut reacts = Vec::new();
        for line in input.lines() {
            let mut parts = line.split(" => ");
            let mut ingrs = Vec::new();
            for ingr in parts.next().unwrap().split(", ") {
                let mut parts = ingr.split(" ");
                let quant = parts.next().unwrap().parse::<u64>().unwrap();
                let name = parts.next().unwrap();
                debug_assert!(parts.next().is_none());
                let idx = map.entry(name).or_insert_with(|| {
                    reacts.push((Vec::new(), 0));
                    reacts.len() - 1
                });
                ingrs.push((*idx, quant));
            }

            let mut parts = parts.next().unwrap().split(" ");
            let quant = parts.next().unwrap().parse::<u64>().unwrap();
            let name = parts.next().unwrap();
            debug_assert!(parts.next().is_none());
            let idx = map.entry(name).or_insert_with(|| {
                reacts.push((Vec::new(), 0));
                reacts.len() - 1
            });
            reacts[*idx] = (ingrs, quant);

            debug_assert!(parts.next().is_none());
        }

        let fuel_idx = *map.get("FUEL").unwrap();
        let ore_idx = *map.get("ORE").unwrap();

        let mut used_by_counts = vec![0; reacts.len()];
        let mut queue = VecDeque::from([fuel_idx]);
        while let Some(idx) = queue.pop_front() {
            if used_by_counts[idx] == 0 {
                for (req_idx, _) in reacts[idx].0.iter() {
                    queue.push_back(*req_idx);
                }
            }
            used_by_counts[idx] += 1;
        }

        let mut order = Vec::new();
        let mut queue = VecDeque::from([fuel_idx]);
        while let Some(idx) = queue.pop_front() {
            used_by_counts[idx] -= 1;
            if used_by_counts[idx] == 0 {
                order.push(idx);
                for (react_idx, _) in reacts[idx].0.iter().copied() {
                    queue.push_back(react_idx);
                }
            }
        }
        debug_assert_eq!(order.last(), Some(&ore_idx));

        let run = |fuel_amount: u64| -> u64 {
            let mut needed = vec![0; reacts.len()];
            needed[fuel_idx] = fuel_amount;
            for idx in order.iter().copied() {
                if idx == ore_idx {
                    return needed[idx];
                }

                let (reactants, product_quant) = &reacts[idx];
                let reaction_count = (needed[idx] - 1) / product_quant + 1;
                for (reactant_idx, reactant_quant) in reactants.iter().copied() {
                    needed[reactant_idx] += reactant_quant * reaction_count;
                }
            }
            unreachable!();
        };

        let sol1 = run(1);

        let mut left = sol1;
        let mut right = ORE;
        while right - left > 1 {
            let middle = (left + right) / 2;
            let cost = run(middle);
            if cost <= ORE {
                left = middle;
            } else {
                right = middle;
            }
        }
        let sol2 = left;

        [Solution::U64(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(4), Solution::U64(ORE / 3)],
            input: "2 ORE => 2 A
3 A => 1 FUEL",
        },
        Example {
            solution: [Solution::U32(10), Solution::U64(ORE / 8 - 1)],
            input: "2 ORE => 2 A
3 A => 3 B
8 B => 1 FUEL",
        },
        Example {
            solution: [Solution::U32(31), Solution::None],
            input: "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL",
        },
        Example {
            solution: [Solution::U32(165), Solution::None],
            input: "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL",
        },
        Example {
            solution: [Solution::U32(13312), Solution::U32(82892753)],
            input: "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT",
        },
        Example {
            solution: [Solution::U32(180697), Solution::U32(5586022)],
            input: "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF",
        },
    ];
}
