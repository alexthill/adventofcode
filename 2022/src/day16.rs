use crate::Solution;
use crate::utils::iter_parse_u32;

pub const SOLUTION: Solution = Solution::U32((1820, 2602));

#[derive(Debug)]
struct Valve {
    id: u32,
    flow: u32,
    leads: Vec<usize>,
}

pub fn solve(input: String) -> Solution {
    let mut valves = Vec::new();
    let mut nonzero_valves = Vec::new();
    let mut iter = input.bytes();
    let mut start = usize::MAX;
    
    loop {
        if iter.nth("Valve ".len() - 1).is_none() {
            break;
        }
        let id = iter.next().unwrap() as u32 * 256 + iter.next().unwrap() as u32;
        iter.nth(" has flow rate=".len() - 1);
        let flow = iter_parse_u32(&mut iter).0.unwrap();
        
        if iter.nth(" tunnels lead to valves".len() - 1).unwrap() == b's' {
            iter.next().unwrap();
        }
        
        let mut leads = Vec::new();
        loop {
            let id = iter.next().unwrap() as u32 * 256 + iter.next().unwrap() as u32;
            leads.push(id as usize);
            
            if iter.next().unwrap() == b'\n' {
                break;
            }
            iter.next().unwrap();
        }
        
        valves.push(Valve { id, flow, leads });
        if flow > 0 {
            nonzero_valves.push(valves.len() - 1);
        }
        if id == b'A' as u32 * 257 {
            start = valves.len() - 1;
        }
    }
    
    assert_ne!(start, usize::MAX, "Did not find start valve named AA");
    
    for i in 0..valves.len() {
        for j in 0..valves[i].leads.len() {
            let id = valves[i].leads[j] as u32;
            let pos = valves.iter().position(|v| v.id == id).unwrap();
            valves[i].leads[j] = pos;
        }
    }
    
    let mut dist = (0..valves.len()).map(|_| { let mut vec = Vec::new(); vec.resize(valves.len(), u32::MAX / 2); vec }).collect::<Vec<_>>();
    for (from_idx, valve) in valves.iter().enumerate() {
        for &to_idx in valve.leads.iter() {
            dist[from_idx][to_idx] = 1;
        }
        dist[from_idx][from_idx] = 0;
    }
    for k in 0..valves.len() {
        for i in 0..valves.len() {
            for j in 0..valves.len() {
                if dist[i][j] > dist[i][k] + dist[k][j] {
                    dist[i][j] = dist[i][k] + dist[k][j];
                }
            }
        }
    }
    
    //let order = [3, 1, 9, 7, 4, 2];
    let presure1 = calc1::<30>(&valves, &dist, &mut nonzero_valves, start, 0, 0);
    let presure2 = calc2(&valves, &dist, &mut nonzero_valves, start, start, 0, 0);
    
    (presure1, presure2).into()
}

fn calc1<const TIME: u32>(valves: &[Valve], dist: &[Vec<u32>], to_open: &mut Vec<usize>, curr: usize, flow: u32, elapsed: u32) -> u32 {
    debug_assert!(elapsed <= TIME);
    
    if to_open.is_empty() {
        (TIME - elapsed) * flow
    } else {
        let last_idx = to_open.len() - 1;
        let mut max = 0;
        for i in 0..to_open.len() {
            let to = to_open[i];
            let time = dist[curr][to] + 1;
            let new_elapsed = elapsed + time;
            
            let added = if new_elapsed <= TIME {
                to_open.swap_remove(i);
                let new_flow = flow + valves[to].flow;
                let added = time * flow + calc1::<TIME>(valves, dist, to_open, to, new_flow, new_elapsed);
                to_open.push(to);
                to_open.swap(i, last_idx);
                added
            } else {
                (TIME - elapsed) * flow
            };
            
            max = max.max(added);
        }
        
        max
    }
}

fn calc2(valves: &[Valve], dist: &[Vec<u32>], to_open: &mut Vec<usize>, start: usize, curr: usize, flow: u32, elapsed: u32) -> u32 {
    debug_assert!(elapsed <= 26);
    
    if to_open.is_empty() {
        (26 - elapsed) * flow
    } else {
        let last_idx = to_open.len() - 1;
        let mut max = 0;
        for i in 0..to_open.len() {
            let to = to_open[i];
            let time = dist[curr][to] + 1;
            let new_elapsed = elapsed + time;
            
            let added = if new_elapsed <= 26 {
                to_open.swap_remove(i);
                let new_flow = flow + valves[to].flow;
                let added = time * flow + calc2(valves, dist, to_open, start, to, new_flow, new_elapsed);
                to_open.push(to);
                to_open.swap(i, last_idx);
                added
            } else {
                (26 - elapsed) * flow
            };
            
            max = max.max(added);
        }
        
        let val = (26 - elapsed) * flow + calc1::<26>(valves, dist, to_open, start, 0, 0);
        max = max.max(val);
        
        max
    }
}
