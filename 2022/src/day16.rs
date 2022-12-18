use crate::Solution;
use crate::utils::iter_parse_u32;

pub const SOLUTION: Solution = Solution::U32((1820, 2602));

#[derive(Debug)]
struct Valve {
    id: u32,
    leads: Vec<u32>,
}

pub fn solve(input: String) -> Solution {
    let mut valves = Vec::new();
    let mut nonzero_valves = Vec::new();
    let mut iter = input.bytes();
    let mut start = u32::MAX;
    
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
            leads.push(id);
            
            if iter.next().unwrap() == b'\n' {
                break;
            }
            iter.next().unwrap();
        }
        
        if flow > 0 {
            nonzero_valves.push((valves.len() as u32, flow));
        }
        if id == b'A' as u32 * 257 {
            start = valves.len() as u32;
        }
        valves.push(Valve { id, leads });
    }
    
    assert_ne!(start, u32::MAX, "Did not find start valve named AA");
    
    for i in 0..valves.len() {
        for j in 0..valves[i].leads.len() {
            let id = valves[i].leads[j];
            let pos = valves.iter().position(|v| v.id == id).unwrap();
            valves[i].leads[j] = pos as u32;
        }
    }
    
    // this is the Floyd-Warshall algorithm to calculate distances between all valve pairs
    let mut dist = (0..valves.len()).map(|i| {
        let mut vec = Vec::new();
        vec.resize(valves.len(), u8::MAX / 2);
        for &to_idx in valves[i].leads.iter() {
            vec[to_idx as usize] = 1;
        }
        vec[i] = 0;
        vec.into_boxed_slice()
    }).collect::<Box<_>>();
    for k in 0..valves.len() {
        for i in 0..valves.len() {
            for j in 0..valves.len() {
                if dist[i][j] > dist[i][k] + dist[k][j] {
                    dist[i][j] = dist[i][k] + dist[k][j];
                }
            }
        }
    }
    
    // using a decent start estimation of the maximum released pressure saves calculations
    // visit the valves in decending flow order as estimation
    nonzero_valves.sort_unstable_by_key(|&(_, flow)| flow);
    let mut curr = 0;
    let mut flow = 0;
    let mut time = 30;
    let mut pressure = 0;
    for &(to_idx, to_flow) in nonzero_valves.iter().rev() {
        let elapsed = dist[curr as usize][to_idx as usize] as u32 + 1;
        if elapsed < time {
            pressure += elapsed * flow;
            flow += to_flow;
            curr = to_idx;
            time -= elapsed;
        } else {
            pressure += time * flow;
            break;
        }
    }
    
    let pressure1 = calc1(&dist, &mut nonzero_valves, start, 0, 30, 0, pressure);
    let pressure2 = calc2(&dist, &mut nonzero_valves, start, start, 0, 26, 0, pressure1);
    
    (pressure1, pressure2).into()
}

fn calc1(dist: &[Box<[u8]>], to_open: &mut [(u32, u32)], curr: u32, flow: u32, time: u32, pressure: u32, best: u32) -> u32 {
    if to_open.len() > 1 {
        let estimate = to_open.iter().map(|&(idx, flow)| flow * time.saturating_sub(dist[curr as usize][idx as usize] as u32 + 1)).sum::<u32>();
        if pressure + time * flow + estimate <= best {
            return 0;
        }
        
        let last_idx = to_open.len() - 1;
        let mut max = best.max(pressure + time * flow);
        for i in 0..to_open.len() {
            let (to_idx, to_flow) = to_open[i];
            let elapsed = dist[curr as usize][to_idx as usize] as u32 + 1;
            
            if elapsed < time {
                to_open.swap(i, last_idx);
                let new_flow = flow + to_flow;
                let new_pressure = pressure + elapsed * flow;
                let val = calc1(dist, &mut to_open[..last_idx], to_idx, new_flow, time - elapsed, new_pressure, max);
                max = max.max(val);
                to_open.swap(i, last_idx);
            }
        }
        
        max
    } else {
        let (to_idx, to_flow) = to_open[0];
        let elapsed = dist[curr as usize][to_idx as usize] as u32 + 1;
        
        if elapsed < time {
            let new_flow = flow + to_flow;
            pressure + elapsed * flow + (time - elapsed) * new_flow
        } else {
            pressure + time * flow
        }
    }
}

fn calc2(dist: &[Box<[u8]>], to_open: &mut [(u32, u32)], start: u32, curr: u32, flow: u32, time: u32, pressure: u32, best: u32) -> u32 {
    if to_open.len() > 1 {
        let estimate = to_open.iter().map(|&(idx, flow)| flow * time.saturating_sub(dist[curr as usize][idx as usize] as u32 + 1)).sum::<u32>();
        // elephant can release at most as much pressure as self
        if 2 * (pressure + time * flow + estimate) <= best {
            return 0;
        }
        
        let last_idx = to_open.len() - 1;
        let mut max = best.max(pressure + time * flow);
        for i in 0..to_open.len() {
            let (to_idx, to_flow) = to_open[i];
            let elapsed = dist[curr as usize][to_idx as usize] as u32 + 1;
            
            if elapsed < time {
                to_open.swap(i, last_idx);
                let new_flow = flow + to_flow;
                let new_pressure = pressure + elapsed * flow;
                let val = calc2(dist, &mut to_open[..last_idx], start, to_idx, new_flow, time - elapsed, new_pressure, max);
                max = max.max(val);
                to_open.swap(i, last_idx);
            }
        }
        
        let val = calc1(dist, to_open, start, 0, 26, pressure + time * flow, max);
        max = max.max(val);
        
        max
    } else {
        // let the elephant open the last valve
        let (to_idx, to_flow) = to_open[0];
        let elapsed = dist[start as usize][to_idx as usize] as u32 + 1;
        assert!(elapsed <= 26);
        pressure + time * flow + (26 - elapsed) * to_flow
    }
}
