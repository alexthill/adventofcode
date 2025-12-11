use aoc_lib_rust::{Day, Example, Solution};
use std::collections::HashMap;

pub struct Day11;

impl Day for Day11 {

    const PART1: Solution = Solution::U32(658);
    const PART2: Solution = Solution::U64(371113003846800);

    fn solve(input: &str) -> [Solution; 2] {
        let mut nodes = HashMap::new();
        for (i, line) in input.as_bytes().split(|ch| *ch == b'\n').enumerate() {
            let node = &line[0..3];
            nodes.insert(node, i);
        }
        nodes.insert(&b"out"[..], nodes.len());
        let len = nodes.len();

        let mut edges = nodes.values().map(|_| Vec::new()).collect::<Vec<_>>();
        for (i, line) in input.as_bytes().split(|ch| *ch == b'\n').enumerate() {
            for adj in line[5..].split(|ch| *ch == b' ') {
                let adj_idx = *nodes.get(adj).unwrap();
                edges[i].push(adj_idx);
            }
        }

        let you = *nodes.get(&b"you"[..]).unwrap();
        let out = *nodes.get(&b"out"[..]).unwrap();

        // Topologically sort the nodes.
        fn visit(edges: &[Vec<usize>], marks: &mut [u32], n: usize, sorted: &mut Vec<usize>) {
            match marks[n] {
                1 => return,
                2 => panic!("cyclic graph"),
                _ => {}
            }
            marks[n] = 2;
            for m in edges[n].iter() {
                visit(edges, marks, *m, sorted);
            }
            marks[n] = 1;
            sorted.push(n);
        }
        let mut marks = vec![0; len];
        let mut sorted = Vec::new();
        while let Some((n, _)) = marks.iter().enumerate().find(|(_, mark)| **mark == 0) {
            visit(&edges, &mut marks, n, &mut sorted);
        }

        let mut counts = vec![0; len];
        counts[you] = 1;
        for &v in sorted.iter().rev() {
            for &u in edges[v].iter() {
                counts[u] += counts[v];
            }
        }
        let sol1 = counts[out];

        // First example does not contain svr, fft and dac nodes.
        let sol2 = if let Some(svr) = nodes.get(&b"svr"[..]).copied() {
            let fft = *nodes.get(&b"fft"[..]).unwrap();
            let dac = *nodes.get(&b"dac"[..]).unwrap();

            // For each node
            // - [0] contains the count passing through neither fft nor dac
            // - [1] passing through only fft
            // - [2] passing through only dac
            // - [3] passing through both fft and dac
            let mut counts = vec![[0; 4]; len];
            counts[svr][0] = 1;
            for &v in sorted.iter().rev() {
                for &u in edges[v].iter() {
                    let [cv, cu] = counts.get_disjoint_mut([v, u]).unwrap();
                    if u == fft {
                        cu[1] += cv[0] + cv[1];
                        cu[3] += cv[3] + cv[2];
                    } else if u == dac {
                        cu[2] += cv[0] + cv[2];
                        cu[3] += cv[3] + cv[1];
                    } else {
                        for (cu, cv) in cu.iter_mut().zip(cv.iter_mut()) {
                            *cu += *cv;
                        }
                    }
                }
            }
            counts[out][3]
        } else {
            0
        };

        [Solution::U32(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(5), Solution::None],
            input: "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out",
        },
        Example {
            solution: [Solution::U32(1), Solution::U32(2)],
            input: "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
you: out",
        },
    ];
}
