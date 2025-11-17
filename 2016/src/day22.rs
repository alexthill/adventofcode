use aoc_lib_rust::{next, Day, Example, Solution};

#[derive(Debug, Default, Clone, Copy)]
struct Node {
    pos: [usize; 2],
    used: u32,
    avail: u32,
}

pub struct Day22;

impl Day for Day22 {

    const PART1: Solution = Solution::U32(993);
    const PART2: Solution = Solution::U32(202);

    fn solve(input: &str) -> [Solution; 2] {
        let nodes = input.lines().skip(2).map(|line| {
            let mut parts = line.split_ascii_whitespace();
            let mut name_parts = next!(parts).split('-');
            assert_eq!(name_parts.next(), Some("/dev/grid/node"));
            let x = next!(name_parts)[1..].parse().unwrap();
            let y = next!(name_parts)[1..].parse().unwrap();
            let _cap = next!(parts);
            let used = next!(parts);
            let avail = next!(parts);
            Node {
                pos: [x, y],
                used: used[..used.len() - 1].parse().unwrap(),
                avail: avail[..avail.len() - 1].parse().unwrap(),
            }
        }).collect::<Vec<_>>();

        let mut nodes_by_avail = nodes.clone();
        nodes_by_avail.sort_unstable_by_key(|node| node.avail);

        let sol1 = nodes.iter().map(|node| {
            if node.used == 0 {
                return 0;
            }
            let mut idx = match nodes_by_avail
                .binary_search_by_key(&node.used, |node2| node2.avail)
            {
                Ok(idx) => idx,
                Err(idx) => idx,
            };
            while idx != 0 && nodes_by_avail[idx - 1].avail >= node.used {
                idx -= 1;
            }
            (nodes_by_avail.len() - idx) as u32 - (node.avail >= node.used) as u32
        }).sum();

        let max_x = nodes.iter().map(|node| node.pos[0]).max().unwrap();
        let target = nodes.iter().find(|node| node.pos[0] == max_x).unwrap();
        let free = nodes.iter().find(|node| node.used == 0).unwrap();
        let wall = nodes.iter().find(|node| node.used > 2 * target.used).unwrap();
        let wall_min_x = nodes.iter().filter_map(|node| {
            if node.used > 2 * target.used { Some(node.pos[0]) } else { None }
        }).min().unwrap();

        // Assumptions:
        // - there is one free node
        // - "walls" have at least double the used size of the target node
        // - "walls" block one row from "wall_min_x" to the right edge
        // - the "free" space can be moved freely around all none-wall nodes
        let sol2 = if free.pos[1] <= wall.pos[1] || free.pos[0] < wall.pos[0] {
            // Wall is irrelevant (like in the example).
            // Just move free up and right and then move target to the left.
            free.pos[1] + max_x - free.pos[0] + 5 * (max_x - 1)
        } else {
            free.pos[1] + max_x - free.pos[0] + 5 * (max_x - 1)
                + 2 * (free.pos[0] - wall_min_x + 1)
        } as u32;

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(7), Solution::U32(7)],
            input: "root@ebhq-gridcenter# df -h
                Filesystem            Size  Used  Avail  Use%
/dev/grid/node-x0-y0   10T    8T     2T   80%
/dev/grid/node-x0-y1   11T    6T     5T   54%
/dev/grid/node-x0-y2   32T   28T     4T   87%
/dev/grid/node-x1-y0    9T    7T     2T   77%
/dev/grid/node-x1-y1    8T    0T     8T    0%
/dev/grid/node-x1-y2   11T    7T     4T   63%
/dev/grid/node-x2-y0   10T    6T     4T   60%
/dev/grid/node-x2-y1    9T    8T     1T   88%
/dev/grid/node-x2-y2    9T    6T     3T   66%",
        },
    ];
}
