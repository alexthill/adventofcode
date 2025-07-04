use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::{next, next_parse};

#[derive(Debug, Clone)]
struct Group<'a> {
    units: u32,
    hp: u32,
    immune: Vec<&'a str>,
    weak: Vec<&'a str>,
    ad: u32,
    ty: &'a str,
    initiative: u32,
    selection: Option<usize>,
    selected: bool,
}

impl Group<'_> {
    fn effective_power(&self) -> u32 {
        self.units * self.ad
    }

    fn dmg_against(&self, other: &Self) -> u32 {
        if other.immune.contains(&self.ty) {
            0
        } else if other.weak.contains(&self.ty) {
            self.effective_power() * 2
        } else {
            self.effective_power()
        }
    }
}

pub struct Day24;

impl Day for Day24 {

    const PART1: Solution = Solution::U32(22083);
    const PART2: Solution = Solution::U32(8975);

    fn solve(input: &str) -> [Solution; 2] {
        fn parse(s: &str) -> Vec<Group> {
            s.split('\n').skip(1).map(|line| {
                let mut parts = line.split(['(', ')']);
                let mut parts1 = next!(parts).split(' ');
                let (parts_immune, parts_weak) = if let Some(part2) = parts.next() {
                    let mut parts2 = part2.split("; ");
                    match (parts2.next(), parts2.next()) {
                        (Some(a), Some(b)) if a.starts_with("immune") =>
                            (a["immune to ".len()..].split(", "), b["weak to ".len()..].split(", ")),
                        (Some(a), Some(b)) if a.starts_with("weak") =>
                            (b["immune to ".len()..].split(", "), a["weak to ".len()..].split(", ")),
                        (Some(a), None) if a.starts_with("immune") =>
                            (a["immune to ".len()..].split(", "), "".split(" ")),
                        (Some(a), None) if a.starts_with("weak") =>
                            ("".split(" "), a["weak to ".len()..].split(", ")),
                        _ => unreachable!(),
                    }
                } else {
                    ("".split(" "), "".split(" "))
                };
                let mut parts3 = if let Some(part3) = parts.next() {
                    part3.split(' ').skip(6)
                } else {
                    line.split(' ').skip(12)
                };
                debug_assert!(parts.next().is_none());

                Group {
                    units: next_parse!(parts1, u32),
                    hp: next_parse!(parts1.skip(3), u32),
                    immune: parts_immune.collect(),
                    weak: parts_weak.collect(),
                    ad: next_parse!(parts3, u32),
                    ty: next!(parts3),
                    initiative: next_parse!(parts3.skip(3), u32),
                    selection: None,
                    selected: false,
                }
            }).collect()
        }

        let (a, b) = input.split_once("\n\n").unwrap();
        let armies = [parse(a), parse(b)];
        let attack_order = {
            let mut indices = (0..2).flat_map(|army_idx| {
                (0..armies[army_idx].len()).map(move |group_idx| (army_idx, group_idx))
            }).collect::<Vec<(usize, usize)>>();
            indices.sort_by(|a, b| {
                let (a, b) = (&armies[a.0][a.1], &armies[b.0][b.1]);
                a.initiative.cmp(&b.initiative).reverse()
            });
            indices
        };

        fn battle(armies: &mut [Vec<Group>; 2], attack_order: &[(usize, usize)]) -> (Option<bool>, u32) {
            loop {
                for army_idx in 0..2 {
                    let groups = &armies[army_idx];
                    let mut indices = groups.iter().enumerate()
                        .filter_map(|(idx, group)| if group.units == 0 { None } else { Some(idx) })
                        .collect::<Vec<usize>>();
                    indices.sort_by(|&a, &b| {
                        let (a, b) = (&groups[a], &groups[b]);
                        a.effective_power()
                            .cmp(&b.effective_power())
                            .then(a.initiative.cmp(&b.initiative))
                            .reverse()
                    });
                    for &idx in indices.iter() {
                        if let Some((i, g)) = armies[army_idx ^ 1].iter().enumerate()
                            .filter(|(_, group)| !group.selected && group.units > 0)
                            .max_by_key(|(_, group)| {
                                let dmg = armies[army_idx][idx].dmg_against(&group);
                                (dmg, group.effective_power(), group.initiative)
                            })
                        {
                            if armies[army_idx][idx].dmg_against(g) != 0 {
                                armies[army_idx][idx].selection = Some(i);
                                armies[army_idx ^ 1][i].selected = true;
                            }
                        }
                    }
                }
                let mut attacked = false;
                for (army_idx, group_idx) in attack_order.iter().cloned() {
                    let attacker = &armies[army_idx][group_idx];
                    let Some(selection) = attacker.selection else {
                        continue;
                    };
                    if attacker.units == 0 {
                        armies[army_idx ^ 1][selection].selected = false;
                        continue;
                    }
                    let defender = &armies[army_idx ^ 1][selection];
                    let dmg = attacker.dmg_against(defender);
                    let kill = (dmg / defender.hp).min(defender.units);
                    armies[army_idx][group_idx].selection = None;
                    armies[army_idx ^ 1][selection].selected = false;
                    armies[army_idx ^ 1][selection].units -= kill;
                    if kill != 0 {
                        attacked = true;
                    }
                }
                if !attacked {
                    break (None, 0);
                }
                if armies[0].iter().all(|group| group.units == 0) {
                    break (Some(false), armies[1].iter().map(|group| group.units).sum());
                }
                if armies[1].iter().all(|group| group.units == 0) {
                    break (Some(true), armies[0].iter().map(|group| group.units).sum());
                }
            }
        }

        let sol1 = battle(&mut armies.clone(), &attack_order).1;

        // l is guessed, r is the value from the example
        let (mut l, mut r) = (50, 1570);
        let sol2 = loop {
            let m = (l + r - 1) / 2 + 1;
            let mut armies = armies.clone();
            for group in armies[0].iter_mut() {
                group.ad += m;
            }
            let (winner, units) = battle(&mut armies, &attack_order);
            println!("{winner:?} {units} {m}");
            if r == m {
                break units;
            }
            if winner == Some(true) {
                r = m;
            } else {
                l = m;
            }
        };

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(5216), Solution::U32(51)],
            input: "Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4",
        },
    ];
}
