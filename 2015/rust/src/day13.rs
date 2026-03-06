use aoc_lib_rust::{Day, Example, Solution};

pub struct Day13;

impl Day for Day13 {

    const PART1: Solution = Solution::U32(664);
    const PART2: Solution = Solution::U32(640);

    fn solve(input: &str) -> [Solution; 2] {
        let mut happiness = [[0_i32; 26]; 26];
        let mut persons = Vec::new();
        for line in input.lines() {
            let person = (line.as_bytes()[0] - b'A') as usize;
            let parts = line.split(' ');

            let mut parts = parts.skip(2);
            let gain = parts.next().unwrap() == "gain";
            let value = parts.next().unwrap().parse().unwrap();

            let mut parts = parts.skip(6);
            let neighbor = parts.next().unwrap().as_bytes()[0] - b'A';

            happiness[person][neighbor as usize] = if gain { value } else { -value };

            if persons.last().is_none_or(|last| *last != person) {
                persons.push(person);
            }
        }

        fn max_happiness(
            persons: &[usize],
            happiness: &[[i32; 26]; 26],
            visited: &mut [bool],
            order: &mut Vec<usize>,
            result: i32,
        ) -> i32 {
            if order.len() == persons.len() {
                let first = *order.first().unwrap();
                let last = *order.last().unwrap();
                return result + happiness[first][last] + happiness[last][first];
            }

            let mut max = i32::MIN;
            for &person in persons {
                if visited[person] {
                    continue;
                }

                let change = if let Some(last) = order.last().copied() {
                    happiness[last][person] + happiness[person][last]
                } else {
                    0
                };

                visited[person] = true;
                order.push(person);

                let new = max_happiness(persons, happiness, visited, order, result + change);
                max = max.max(new);

                order.pop();
                visited[person] = false;
            }

            max
        }

        let mut visited = [false; 26];
        let sol1 = max_happiness(&persons, &happiness, &mut visited, &mut Vec::new(), 0);

        persons.push(25);
        let mut visited = [false; 26];
        let sol2 = max_happiness(&persons, &happiness, &mut visited, &mut Vec::new(), 0);

        [Solution::U32(sol1 as _), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(330), Solution::None],
            input: "Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.",
        },
    ];
}
