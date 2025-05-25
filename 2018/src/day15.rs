use aoc_lib_rust::{Day, Example, Solution};
use std::collections::VecDeque;
use std::mem;

#[derive(Debug, Clone, Copy)]
struct Entity {
    attack: u8,
    health: u8,
    is_elf: bool,
    pos: usize,
}

impl Entity {
    fn new(is_elf: bool, pos: usize) -> Self {
        Self { attack: 3, health: 200, is_elf, pos }
    }
}

#[derive(Debug, Clone, Copy)]
enum Cell {
    Empty,
    Wall,
    Entity(usize),
}

impl Default for Cell {
    fn default() -> Self { Self::Empty }
}

fn simulate(
    map: &mut [Cell],
    w: usize,
    visited: &mut [bool],
    entities: &mut [Entity],
    alive: [u32; 2],
    order: &mut [usize],
) -> (bool, u32, u32) {
    assert_eq!(map.len(), visited.len());

    let mut alive = alive;
    let mut round = 1;
    let full_rounds = 'outer: loop {
        order.sort_unstable_by_key(|&idx| entities[idx].pos);
        for (order_i, &i) in order.iter().enumerate() {
            if entities[i].health == 0 {
                continue;
            }
            visited.fill(false);
            let mut found = None;
            let mut queue = VecDeque::from([(entities[i].pos, 0, None)]);
            visited[entities[i].pos] = true;
            while let Some((pos, depth, start)) = queue.pop_front() {
                if found.is_some_and(|(_, d, _)| d < depth) {
                    break;
                }
                for dir in [-(w as i32), -1, 1, w as i32] {
                    let new_pos = (pos as i32 + dir) as usize;
                    let start = start.unwrap_or(new_pos);
                    match map[new_pos] {
                        Cell::Empty if !visited[new_pos] => {
                            queue.push_back((new_pos, depth + 1, Some(start)));
                            visited[new_pos] = true;
                        }
                        Cell::Entity(j) if entities[i].is_elf != entities[j].is_elf => {
                            #[allow(clippy::nonminimal_bool)]
                            if !found.is_some_and(|(p, _, _)| p < pos) {
                                found = Some((pos, depth, start));
                            }
                        }
                        _ => {}
                    }
                }
            }

            let Some((_, found_depth, found_start)) = found else { continue };
            if found_depth > 0 {
                map[found_start] = mem::take(&mut map[entities[i].pos]);
                entities[i].pos = found_start;
            }
            if let Some(idx) = [-(w as i32), -1, 1, w as i32].into_iter()
                .filter_map(|dir| {
                    let new_pos = (entities[i].pos as i32 + dir) as usize;
                    if let Cell::Entity(idx) = map[new_pos] {
                        if entities[idx].is_elf == entities[i].is_elf {
                            None
                        } else {
                            Some(idx)
                        }
                    } else {
                        None
                    }
                })
                .min_by_key(|idx| entities[*idx].health)
            {
                entities[idx].health = entities[idx].health.saturating_sub(entities[i].attack);
                if entities[idx].health == 0 {
                    map[entities[idx].pos] = Cell::Empty;
                    alive[entities[idx].is_elf as usize] -= 1;
                    if alive[entities[idx].is_elf as usize] == 0 {
                        break 'outer if order_i == order.len() - 1 { round } else { round - 1 };
                    }
                }
            }
        }
        round += 1;
    };
    let health_left = entities.iter().map(|entity| entity.health as u32).sum::<u32>();
    let elves_won = alive[1] != 0;
    (elves_won, full_rounds, health_left)
}

pub struct Day15;

impl Day for Day15 {

    const PART1: Solution = Solution::U32(245280);
    const PART2: Solution = Solution::U32(74984);

    fn solve(input: &str) -> [Solution; 2] {
        let w = input.chars().position(|c| c == '\n').unwrap();
        let mut map = Vec::new();
        let mut entities = Vec::new();
        let mut pos = 0;
        for line in input.lines() {
            for c in line.chars() {
                match c {
                    '.' => map.push(Cell::Empty),
                    '#' => map.push(Cell::Wall),
                    'E' => {
                        map.push(Cell::Entity(entities.len()));
                        entities.push(Entity::new(true, pos));
                    }
                    'G' => {
                        map.push(Cell::Entity(entities.len()));
                        entities.push(Entity::new(false, pos));
                    }
                    _ => unreachable!(),
                }
                if c != '\n' {
                    pos += 1;
                }
            }
        }
        let mut visited = vec![false; map.len()];
        let mut order = (0..entities.len()).collect::<Vec<_>>();
        let map_og = map.clone();
        let entities_og = entities.clone();
        let alive = [false, true].map(|is_elf| {
            entities.iter().filter(|e| e.is_elf == is_elf).count() as u32
        });

        let (_, full_rounds, health_left)
            = simulate(&mut map, w, &mut visited, &mut entities, alive, &mut order);
        let sol1 = full_rounds * health_left;

        let (mut l, mut r) = (4 + alive[0].saturating_sub(alive[1]), 36);
        let sol2 = loop {
            let m = (l + r) / 2;
            map.copy_from_slice(&map_og);
            visited.fill(false);
            entities.copy_from_slice(&entities_og);
            for entity in entities.iter_mut().filter(|entity| entity.is_elf) {
                entity.attack = m as _;
            }
            let (elves_won, full_rounds, health_left)
                = simulate(&mut map, w, &mut visited, &mut entities, alive, &mut order);
            let ok = elves_won && entities.iter().all(|entity| !entity.is_elf || entity.health != 0);
            if l == r {
                break full_rounds * health_left;
            }
            if ok {
                r = m;
            } else {
                l = m + 1;
            }
        };

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(27730), Solution::U32(4988)],
            input: "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######",
        },
        Example {
            solution: [Solution::U32(36334), Solution::None],
            input: "#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######",
        },
        Example {
            solution: [Solution::U32(39514), Solution::U32(31284)],
            input: "#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######",
        },
    ];
}
