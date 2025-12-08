use aoc_lib_rust::{next_parse, Day, Example, Solution};
use std::collections::LinkedList;

pub struct Day08;

impl Day for Day08 {

    const PART1: Solution = Solution::U32(127551);
    const PART2: Solution = Solution::U32(2347225200);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let boxes = input.lines().map(|line| {
            let mut parts = line.split(',');
            [next_parse!(parts, f32), next_parse!(parts, f32), next_parse!(parts, f32)]
        }).collect::<Vec<_>>();

        let mut dists = Vec::with_capacity(boxes.len().pow(2));
        for (i, a) in boxes.iter().enumerate() {
            for (j, b) in boxes.iter().enumerate().skip(i + 1) {
                let dist = a.iter().zip(b).map(|(a, b)| (a - b).powi(2)).sum::<f32>().sqrt();
                dists.push((i, j, dist));
            }
        }

        // Sorting all pairwise distances would degrade performance.
        // So we take some guess at how many distances we will need and just sort them.
        let nth = if example { dists.len() - 1 } else { boxes.len() * 10 };
        let (dists, _, _) = dists.select_nth_unstable_by(nth, |a, b| f32::total_cmp(&a.2, &b.2));
        dists.sort_unstable_by(|a, b| f32::total_cmp(&a.2, &b.2));

        let mut components = Vec::new();
        let mut box_to_comp = Vec::new();
        for i in 0..boxes.len() {
            components.push(LinkedList::from([i]));
            box_to_comp.push(i);
        }

        let max_connections = if example { 10 } else { 1000 };
        let mut sol1 = 0;
        let mut sol2 = 0;
        for (i, (a, b, _)) in dists.iter().enumerate() {
            if i == max_connections {
                let mut counts = components.iter().map(|comp| comp.len()).collect::<Vec<_>>();
                counts.sort_unstable_by(|a, b| a.cmp(b).reverse());
                sol1 = counts.iter().take(3).product();
            }

            let ca = box_to_comp[*a];
            let cb = box_to_comp[*b];
            if let Ok([list_a, list_b]) = components.get_disjoint_mut([ca, cb]) {
                list_a.append(list_b);
                if list_a.len() == boxes.len() {
                    sol2 = boxes[*a][0] as u64 * boxes[*b][0] as u64;
                    break;
                }
                for bx in list_a.iter() {
                    box_to_comp[*bx] = ca;
                }
            }
        }

        [Solution::U32(sol1 as _), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(40), Solution::U32(25272)],
            input: "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689",
        },
    ];
}
