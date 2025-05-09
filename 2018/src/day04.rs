use aoc_lib_rust::{Day, Example, Solution};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
enum Action {
    BeginShift(u32),
    FallAsleep,
    WakeUp,
}

#[derive(Debug, Clone, Copy)]
struct Entry {
    month: u8,
    day: u8,
    hour: u8,
    min: u8,
    action: Action,
}

pub struct Day04;

impl Day for Day04 {

    const PART1: Solution = Solution::U32(115167);
    const PART2: Solution = Solution::U32(32070);

    fn solve(input: &str) -> [Solution; 2] {
        let mut entries = input.lines().map(|line| {
            let (time, action) = line.split_once(']').unwrap();

            let mut time_parts = time.split(['-', ' ', ':']);
            assert_eq!(time_parts.next(), Some("[1518"));
            let month = time_parts.next().unwrap().parse().unwrap();
            let day = time_parts.next().unwrap().parse().unwrap();
            let hour = time_parts.next().unwrap().parse().unwrap();
            let min = time_parts.next().unwrap().parse().unwrap();

            let action = if let Some(tag_pos) = action.find('#') {
                let space_pos = action[tag_pos..].find(' ').unwrap();
                Action::BeginShift(action[tag_pos + 1..tag_pos + space_pos].parse().unwrap())
            } else if action.as_bytes()[1] == b'f' {
                Action::FallAsleep
            } else if action.as_bytes()[1] == b'w' {
                Action::WakeUp
            } else {
                unreachable!()
            };

            Entry { month, day, hour, min, action }
        }).collect::<Vec<_>>();
        entries.sort_unstable_by_key(|entry| (entry.month, entry.day, entry.hour, entry.min));

        let mut guards = HashMap::new();
        let Action::BeginShift(first_guard_id) = entries[0].action else { unreachable!() };
        let mut guard = guards.entry(first_guard_id).or_insert((0, [0; 59]));
        let mut felt_asleep_min = 0;
        for entry in entries[0..].iter() {
            match entry.action {
                Action::BeginShift(id) => {
                    guard = guards.entry(id).or_insert((0, [0; 59]));
                }
                Action::FallAsleep => felt_asleep_min = entry.min,
                Action::WakeUp => {
                    guard.0 += (entry.min - felt_asleep_min) as u32;
                    for i in felt_asleep_min..entry.min {
                        guard.1[i as usize] += 1;
                    }
                }
            }
        }

        let (best_guard, (_, sleep_times)) = guards.iter()
            .max_by_key(|(_, (time, _))| time)
            .unwrap();
        let (best_min, _) = sleep_times.iter().enumerate()
            .max_by_key(|(_, time)| *time)
            .unwrap();
        let sol1 = best_guard * best_min as u32;

        let (best_guard, (_, best_min)) = guards.iter()
            .map(|(id, (_, sleep_times))| {
                let (best_min, time) = sleep_times.iter().enumerate()
                    .max_by_key(|(_, time)| *time)
                    .unwrap();
                (id, (time, best_min))
            })
            .max_by_key(|(_, (time, _))| *time)
            .unwrap();
        let sol2 = best_guard * best_min as u32;

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(240), Solution::None],
            input: "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up",
        },
    ];
}
