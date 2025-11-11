use aoc_lib_rust::{next, Day, Example, Solution};
use std::collections::HashSet;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
struct Floor {
    items: u16,
}

impl Floor {
    fn is_empty(&self) -> bool {
        self.items == 0
    }

    fn is_bad(&self) -> bool {
        let microchips = self.items & 0xff;
        let generators = self.items >> 8;
        (microchips & !generators != 0) && generators != 0
    }

    fn has_item(&self, id: u16) -> bool {
        self.items & (1 << id) != 0
    }

    fn set_item(&mut self, id: u16) {
        self.items |= 1 << id;
    }

    fn rm_item(&mut self, id: u16) {
        self.items &= !(1 << id);
    }

    fn move_item_to(mut floors: [Self; 4], from: usize, to: usize, id: u16) -> [Self; 4] {
        floors[from].rm_item(id);
        floors[to].set_item(id);
        floors
    }
}

pub struct Day11;

impl Day for Day11 {

    const PART1: Solution = Solution::U32(33);
    const PART2: Solution = Solution::U32(57);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        fn parse_floor<'a>(input: &'a str, elements: &mut Vec<&'a str>) -> Floor {
            let mut floor = Floor::default();
            let mut prev = "";
            for part in input.split(' ') {
                let part = part.trim_end_matches(['.', ',']);
                if part == "microchip" || part == "generator" {
                    let el = if part == "microchip" {
                        next!(prev.split('-'))
                    } else {
                        prev
                    };
                    let el_id = if let Some(pos) = elements.iter().position(|e| *e == el) {
                        pos
                    } else {
                        elements.push(el);
                        elements.len() - 1
                    } as u16;
                    match part {
                        "microchip" => floor.set_item(el_id),
                        "generator" => floor.set_item(el_id + 8),
                        _ => unreachable!(),
                    }
                }
                prev = part;
            }
            floor
        }

        let mut elements = Vec::new();
        let mut floors = [Floor::default(); 4];
        let mut lines = input.lines();
        floors[0] =  parse_floor(next!(lines), &mut elements);
        floors[1] =  parse_floor(next!(lines), &mut elements);
        floors[2] =  parse_floor(next!(lines), &mut elements);
        assert!(elements.len() <= 5, "expected 5 different elements, found {}", elements.len());

        fn solve_helper(
            floors: [Floor; 4],
            floor: usize,
            visited: &mut HashSet<([Floor; 4], u8)>,
            next_states: &mut Vec<([Floor; 4], usize)>,
        ) {
            if visited.contains(&(floors, floor as _)) {
                return;
            }
            visited.insert((floors, floor as _));
            // A further optimization would be to regard all pairs on a floor as
            // interchangeable and restrict moves based on this.
            if floor != 3 {
                let next_floor = floor + 1;
                let mut moved_two = false;
                for item1 in (0..16).filter(|item| floors[floor].has_item(*item)) {
                    let floors = Floor::move_item_to(floors, floor, next_floor, item1);
                    for item2 in (item1 + 1..16).filter(|item| floors[floor].has_item(*item)) {
                        let floors = Floor::move_item_to(floors, floor, next_floor, item2);
                        if !floors[floor].is_bad() && !floors[next_floor].is_bad() {
                            next_states.push((floors, next_floor));
                            moved_two = true;
                        }
                    }
                    // Only try to move one item up if we cannot move two items up.
                    if !moved_two && !floors[floor].is_bad() && !floors[next_floor].is_bad() {
                        next_states.push((floors, next_floor));
                    }
                }
            }
            if floor != 0 {
                let next_floor = floor - 1;
                for item1 in (0..16).filter(|item| floors[floor].has_item(*item)) {
                    let floors = Floor::move_item_to(floors, floor, next_floor, item1);
                    if !floors[floor].is_bad() && !floors[next_floor].is_bad() {
                        next_states.push((floors, next_floor));
                        continue;
                    }
                    // Only try to move two items down if we cannot move one item down.
                    for item2 in (item1 + 1..16).filter(|item| floors[floor].has_item(*item)) {
                        let floors = Floor::move_item_to(floors, floor, next_floor, item2);
                        if !floors[floor].is_bad() && !floors[next_floor].is_bad() {
                            next_states.push((floors, next_floor));
                        }
                    }
                }
            }
        }

        fn solve(floors: [Floor; 4]) -> u32 {
            let mut visited = HashSet::new();
            let mut states = vec![(floors, 0)];
            let mut next_states = Vec::new();
            for depth in 0..1000 {
                for (floors, floor) in states.drain(..) {
                    if floor == 3 && floors[0..3].iter().all(|floor| floor.is_empty()) {
                        return depth;
                    }
                    solve_helper(floors, floor, &mut visited, &mut next_states);
                }
                std::mem::swap(&mut states, &mut next_states);
            }
            panic!("no solution found after depth 1000");
        }

        let sol1 = solve(floors);

        floors[0].set_item(5);
        floors[0].set_item(5 + 8);
        floors[0].set_item(6);
        floors[0].set_item(6 + 8);
        let sol2 = if example { 0 } else { solve(floors) };

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(11), Solution::None],
            input: "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.",
        },
    ];
}
