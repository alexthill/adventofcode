use aoc_lib_rust::{next_parse, Day, Example, Solution};

#[derive(Debug)]
struct Component {
    a: u8,
    b: u8,
    used: bool,
}

impl Component {
    fn strength(&self) -> u8 {
        if self.used {
            self.a + self.b
        } else {
            0
        }
    }
}

pub struct Day24;

impl Day for Day24 {

    const PART1: Solution = Solution::U32(1656);
    const PART2: Solution = Solution::U32(1642);

    fn solve(input: &str) -> [Solution; 2] {
        let mut components = input.lines().map(|line| {
            let mut parts = line.split('/');
            let a = next_parse!(parts, u8);
            let b = next_parse!(parts, u8);
            Component { a, b, used: false }
        }).collect::<Vec<_>>();
        const ARRAY_REPEAT_VALUE: Vec<usize> = Vec::new();
        let mut by_pins = [ARRAY_REPEAT_VALUE; 99];
        for (idx, component) in components.iter().enumerate() {
            by_pins[component.a as usize].push(idx);
            if component.a != component.b {
                by_pins[component.b as usize].push(idx);
            }
        }

        fn find_strongest(components: &mut[Component], by_pins: &[Vec<usize>], next: u8) -> u32 {
            let mut max = 0;
            for &idx in by_pins[next as usize].iter() {
                if components[idx].used {
                    continue;
                }
                components[idx].used = true;
                let next = if components[idx].a == next {
                    components[idx].b
                } else {
                    components[idx].a
                };
                max = max.max(find_strongest(components, by_pins, next));
                components[idx].used = false;
            }
            if max == 0 {
                components.iter().map(|component| component.strength() as u32).sum()
            } else {
                max
            }
        }
        let sol1 = find_strongest(&mut components, &by_pins, 0);

        fn find_longest(components: &mut[Component], by_pins: &[Vec<usize>], next: u8) -> [u32; 2] {
            let mut max = [0; 2];
            for &idx in by_pins[next as usize].iter() {
                if components[idx].used {
                    continue;
                }
                components[idx].used = true;
                let next = if components[idx].a == next {
                    components[idx].b
                } else {
                    components[idx].a
                };
                max = max.max(find_longest(components, by_pins, next));
                components[idx].used = false;
            }
            if max == [0; 2] {
                let length = components.iter().filter(|&component| component.used).count();
                let strength = components.iter().map(|component| component.strength() as u32).sum();
                [length as _, strength]
            } else {
                max
            }
        }
        let sol2 = find_longest(&mut components, &by_pins, 0)[1];

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(31), Solution::U32(19)],
            input: "0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10",
        },
    ];
}
