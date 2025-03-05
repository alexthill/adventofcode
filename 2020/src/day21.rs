use aoc_lib_rust::{Day, Example, Solution};
use std::collections::{HashMap, HashSet};

pub struct Day21;

impl Day for Day21 {

    const PART1: Solution = Solution::U32(2614);
    const PART2: Solution = Solution::Str("qhvz,kbcpn,fzsl,mjzrj,bmj,mksmf,gptv,kgkrhg");

    fn solve(input: &str) -> [Solution; 2] {
        let mut allergs_map = HashMap::<&[u8], HashSet<&[u8]>>::new();
        let mut ingrs_count = HashMap::new();
        for line in input.as_bytes().split(|c| *c == b'\n') {
            let mut parts = line.split(|c| c.is_ascii_whitespace());
            let mut ingrs = HashSet::new();
            while let Some(part) = parts.next() {
                if part[0] == b'(' {
                    break;
                }
                *ingrs_count.entry(part).or_insert(0) += 1;
                ingrs.insert(part);
            }
            for allerg in parts.map(|a| &a[..a.len()-1]) {
                allergs_map.entry(allerg)
                    .and_modify(|e| {
                        *e = e.intersection(&ingrs).copied().collect();
                    })
                    .or_insert(ingrs.clone());
            }
        }

        let mut queue = Vec::<(&[u8], &[u8])>::new();
        let mut found = Vec::new();
        for (allerg, set) in allergs_map.iter() {
            if set.len() == 1 {
                queue.push((allerg, *set.iter().next().unwrap()));
            }
        }
        while let Some((allerg, ingr)) = queue.pop() {
            allergs_map.remove(allerg);
            ingrs_count.remove(ingr);
            found.push((allerg, ingr));
            for (allerg, set) in allergs_map.iter_mut() {
                if set.remove(ingr) && set.len() == 1 {
                    queue.push((allerg, *set.iter().next().unwrap()));
                }
            }
        }
        let sol1 = ingrs_count.into_values().sum();

        found.sort_unstable_by_key(|(allerg, _)| *allerg);
        let mut sol2 = found.into_iter().map(|(_, ingr)| {
            String::from_utf8_lossy(ingr) + ","
        }).collect::<String>();
        sol2.pop().unwrap();

        [Solution::U32(sol1), Solution::String(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(5), Solution::Str("mxmxvkd,sqjhc,fvjkl")],
            input: "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)",
        },
    ];
}
