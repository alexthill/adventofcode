use aoc_lib_rust::{next, Day, Example, Solution};
use std::collections::HashMap;

#[derive(Debug, Default)]
struct Prog<'a> {
    id: usize,
    name: &'a str,
    weight: u32,
    hold_by: Option<usize>,
    holding: Vec<usize>,
}

pub struct Day07;

impl Day for Day07 {

    const PART1: Solution = Solution::Str("eugwuhl");
    const PART2: Solution = Solution::U32(420);

    fn solve(input: &str) -> [Solution; 2] {
        let mut name_to_id = HashMap::new();
        let mut progs = Vec::new();

        fn prog_by_name<'a>(
            name: &'a str, name_to_id: &mut HashMap<&'a str, usize>, progs: &mut Vec<Prog<'a>>
        ) -> usize {
            *name_to_id.entry(name).or_insert_with(|| {
                let id = progs.len();
                progs.push(Prog { id, name, ..Default::default() });
                id
            })
        }

        for line in input.lines() {
            let mut parts = line.split(' ');
            let name = next!(parts);
            let weight = next!(parts);
            let id = prog_by_name(name, &mut name_to_id, &mut progs);
            progs[id].weight = weight[1..weight.len() - 1].parse().unwrap();

            if let Some((_, holding)) = line.split_once(" -> ") {
                for sub_name in holding.split(", ") {
                    let sub_id = prog_by_name(sub_name, &mut name_to_id, &mut progs);
                    progs[sub_id].hold_by = Some(id);
                    progs[id].holding.push(sub_id);
                }
            }
        }

        let mut bottom_prog = None;
        for prog in progs.iter() {
            if prog.hold_by.is_none() {
                bottom_prog = Some(prog);
                break;
            }
        }
        let bottom_prog = bottom_prog.unwrap();
        let sol1 = bottom_prog.name.to_owned();

        fn balance(id: usize, progs: &[Prog<'_>]) -> Result<u32, u32> {
            if let Some(&first_id) = progs[id].holding.first() {
                let w = balance(first_id, progs)?;
                for &sub_id in progs[id].holding[1..].iter() {
                    let v = balance(sub_id, progs)?;
                    if v != w {
                        if progs[id].holding.len() == 2 {
                            unimplemented!();
                        }
                        let third_id = *progs[id].holding[1..].iter()
                            .find(|&&id| id != sub_id)
                            .unwrap();
                        let u = balance(third_id, progs).unwrap();
                        return Err(if u == w {
                            progs[sub_id].weight as i32 + w as i32 - v as i32
                        } else {
                            progs[first_id].weight as i32 + v as i32 - w as i32
                        } as u32);
                    }
                }
                Ok(w * progs[id].holding.len() as u32 + progs[id].weight)
            } else {
                Ok(progs[id].weight)
            }
        }
        let sol2 = balance(bottom_prog.id, &progs).unwrap_err();

        [Solution::String(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::Str("tknk"), Solution::U32(60)],
            input: "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)",
        },
    ];
}
