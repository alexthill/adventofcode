use aoc_lib_rust::{Day, Example, Solution};

pub struct Day07;

impl Day for Day07 {

    const PART1: Solution = Solution::Str("IJLFUVDACEHGRZPNKQWSBTMXOY");
    const PART2: Solution = Solution::U32(1072);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        const ARRAY_REPEAT_VALUE: Option<Vec<usize>> = None;
        let mut steps = [ARRAY_REPEAT_VALUE; 26];
        for line in input.lines() {
            let line = line.as_bytes();
            let pre = (line[b"Step ".len()] - b'A') as usize;
            let step = (line[b"Step X must be finished before step ".len()] - b'A') as usize;
            debug_assert_ne!(pre, step);
            if steps[pre].is_none() {
                steps[pre] = Some(Vec::new());
            }
            if let Some(pres) = steps[step].as_mut() {
                pres.push(pre);
            } else {
                steps[step] = Some(vec![pre]);
            }
        }
        let steps_copy = steps.clone();

        let mut sol1 = String::new();
        'outer: loop {
            for i in 0..steps.len() {
                if let Some(pres) = steps[i].as_mut() {
                    if pres.is_empty() {
                        sol1.push((i as u8 + b'A') as char);
                        steps[i] = None;
                        for pres in steps.iter_mut().filter_map(|pres| pres.as_mut()) {
                            if let Some(idx) = pres.iter().position(|pre| *pre == i) {
                                pres.remove(idx);
                            }
                        }
                        continue 'outer;
                    }
                }
            }
            break;
        }

        let (worker_count, extra_time) = if example { (2, 1) } else { (5, 61) };
        let mut steps = steps_copy;
        let mut workers: [Option<(usize, u32)>; 5] = [None; 5];
        let mut sol2 = 0;
        loop {
            for worker_outer in workers.iter_mut().take(worker_count) {
                let Some(worker) = worker_outer.as_mut() else { continue };
                worker.1 -= 1;
                if worker.1 == 0 {
                    for pres in steps.iter_mut().filter_map(|pres| pres.as_mut()) {
                        if let Some(idx) = pres.iter().position(|pre| *pre == worker.0) {
                            pres.remove(idx);
                        }
                    }
                    *worker_outer = None;
                }
            }
            for (i, step) in steps.iter_mut().enumerate() {
                let Some(pres) = step.as_mut() else { continue };
                if pres.is_empty() {
                    if let Some(worker) = workers.iter_mut().take(worker_count)
                        .find(|w| w.is_none())
                    {
                        *worker = Some((i, extra_time + i as u32));
                        *step = None;
                    }
                }
            }
            if steps.iter().all(|step| step.is_none()) && workers.iter().all(|w| w.is_none()) {
                break;
            }
            sol2 += 1;
        }

        [Solution::String(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::Str("CABDFE"), Solution::U32(15)],
            input: "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.",
        },
    ];
}
