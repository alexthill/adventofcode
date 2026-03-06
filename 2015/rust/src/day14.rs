use aoc_lib_rust::{Day, Example, Solution};

pub struct Day14;

struct Reindeer {
    speed: u32,
    fly: u32,
    rest: u32,
    dist: u32,
    state: i32,
    score: u32,
}

impl Day for Day14 {

    const PART1: Solution = Solution::U32(2660);
    const PART2: Solution = Solution::U32(1256);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let seconds = if example { 1000 } else { 2503 };

        let mut reindeers = input.lines().map(|line| {
            let mut parts = line.split(' ');
            let _name = parts.next().unwrap();

            let mut parts = parts.skip(2);
            let speed: u32 = parts.next().unwrap().parse().unwrap();

            let mut parts = parts.skip(2);
            let fly: u32 = parts.next().unwrap().parse().unwrap();

            let mut parts = parts.skip(6);
            let rest: u32 = parts.next().unwrap().parse().unwrap();

            Reindeer { speed, fly, rest, dist: 0, state: 0, score: 0 }
        }).collect::<Vec<_>>();


        for _ in 0..seconds {
            for reindeer in &mut reindeers {
                if reindeer.state < 0 {
                    reindeer.state += 1;
                } else if reindeer.state as u32 == reindeer.fly {
                    reindeer.state = -(reindeer.rest as i32) + 1;
                } else {
                    reindeer.state += 1;
                    reindeer.dist += reindeer.speed;
                }
            }

            let max_dist = reindeers.iter().max_by_key(|reindeer| reindeer.dist).unwrap().dist;
            for reindeer in &mut reindeers {
                if reindeer.dist == max_dist {
                    reindeer.score += 1;
                }
            }
        }
        let sol1 = reindeers.iter().max_by_key(|reindeer| reindeer.dist).unwrap().dist;
        let sol2 = reindeers.iter().max_by_key(|reindeer| reindeer.score).unwrap().score;

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(1120), Solution::U32(689)],
            input: "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.",
        },
    ];
}
