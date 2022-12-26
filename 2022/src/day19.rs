use crate::Solution;
use crate::utils::iter_parse_u32;

pub const SOLUTION: Solution = Solution::U32((1589, 29348));

#[derive(Debug)]
struct Blueprint {
    id: u32,
    ore_ore: u32,
    clay_ore: u32,
    obs_ore: u32,
    obs_clay: u32,
    geod_ore: u32,
    geod_obs: u32,
}

pub fn solve(input: String) -> Solution {
    let mut iter = input.bytes();
    let mut blueprints = Vec::new();
    
    while iter.nth("Blueprint ".len() - 1).is_some() {
        let id = iter_parse_u32(&mut iter).0.unwrap();
        iter.nth(" Each ore robot costs ".len() - 1).unwrap();
        let ore_ore = iter_parse_u32(&mut iter).0.unwrap();
        iter.nth("ore. Each clay robot costs ".len() - 1).unwrap();
        let clay_ore = iter_parse_u32(&mut iter).0.unwrap();
        iter.nth("ore. Each obsidian robot costs ".len() - 1).unwrap();
        let obs_ore = iter_parse_u32(&mut iter).0.unwrap();
        iter.nth("ore and ".len() - 1).unwrap();
        let obs_clay = iter_parse_u32(&mut iter).0.unwrap();
        iter.nth("clay. Each geode robot costs ".len() - 1).unwrap();
        let geod_ore = iter_parse_u32(&mut iter).0.unwrap();
        iter.nth("ore and ".len() - 1).unwrap();
        let geod_obs = iter_parse_u32(&mut iter).0.unwrap();
        iter.nth("obsidian.\n".len() - 1).unwrap();
        
        blueprints.push(Blueprint { id, ore_ore, clay_ore, obs_ore, obs_clay, geod_ore, geod_obs });
    }
    
    let mut res1 = 0;
    for bp in blueprints.iter() {
        let max_ore_need = bp.ore_ore.max(bp.clay_ore.max(bp.obs_ore.max(bp.geod_ore)));
        let val = calc(bp, &mut [1, 0, 0, 0], [0, 0, 0, 0], 24, 0, max_ore_need);
        res1 += bp.id * val;
    }
    
    let mut res2 = 1;
    for bp in blueprints.iter().take(3) {
        let max_ore_need = bp.ore_ore.max(bp.clay_ore.max(bp.obs_ore.max(bp.geod_ore)));
        let val = calc(bp, &mut [1, 0, 0, 0], [0, 0, 0, 0], 32, 0, max_ore_need);
        res2 *= val;
    }
    
    (res1, res2).into()
}

fn calc(bp: &Blueprint, robots: &mut [u32; 4], res: [u32; 4], time: u32, best: u32, max_ore_need: u32) -> u32 {
    if time == 1 {
        return res[3] + robots[3]
    }
    if time == 2 {
        if res[0] >= bp.geod_ore && res[2] >= bp.geod_obs {
            return res[3] + 2 * robots[3] + 1;
        } else {
            return res[3] + 2 * robots[3];
        }
    }
    if res[3] + (robots[3] + (time - 1) / 2) * time <= best {
        return 0;
    }
    if robots[2] >= bp.geod_obs && robots[0] >= bp.geod_ore {
        return res[3] + (robots[3] + (time - 1) / 2) * time;
    }
    
    let mut max = best.max(res[3] + time * robots[3]);
    if robots[0] < max_ore_need && time > 3 {
        let elapsed = if res[0] < bp.ore_ore {
            (bp.ore_ore - res[0] + robots[0] - 1) / robots[0] + 1
        } else {
            1
        };
        if elapsed < time {
            let mut new_res = res;
            for i in 0..4 { new_res[i] += robots[i] * elapsed; }
            new_res[0] -= bp.ore_ore;
            robots[0] += 1;
            max = max.max(calc(bp, robots, new_res, time - elapsed, max, max_ore_need));
            robots[0] -= 1;
        }
    }
    if robots[1] < bp.obs_clay && time > 5 {
        let elapsed = if res[0] < bp.clay_ore {
            (bp.clay_ore - res[0] + robots[0] - 1) / robots[0] + 1
        } else {
            1
        };
        if elapsed < time {
            let mut new_res = res;
            for i in 0..4 { new_res[i] += robots[i] * elapsed; }
            new_res[0] -= bp.clay_ore;
            robots[1] += 1;
            max = max.max(calc(bp, robots, new_res, time - elapsed, max, max_ore_need));
            robots[1] -= 1;
        }
    }
    if robots[2] < bp.geod_obs && robots[1] > 0 && time > 3 {
        let time1 = if res[0] < bp.obs_ore {
            (bp.obs_ore - res[0] + robots[0] - 1) / robots[0] + 1
        } else {
            1
        };
        let time2 = if res[1] < bp.obs_clay {
            (bp.obs_clay - res[1] + robots[1] - 1) / robots[1] + 1
        } else {
            1
        };
        let elapsed = time1.max(time2);
        if elapsed < time {
            let mut new_res = res;
            for i in 0..4 { new_res[i] += robots[i] * elapsed; }
            new_res[0] -= bp.obs_ore;
            new_res[1] -= bp.obs_clay;
            robots[2] += 1;
            max = max.max(calc(bp, robots, new_res, time - elapsed, max, max_ore_need));
            robots[2] -= 1;
        }
    }
    if robots[2] > 0 {
        let time1 = if res[0] < bp.geod_ore {
            (bp.geod_ore - res[0] + robots[0] - 1) / robots[0] + 1
        } else {
            1
        };
        let time2 = if res[2] < bp.geod_obs {
            (bp.geod_obs - res[2] + robots[2] - 1) / robots[2] + 1
        } else {
            1
        };
        let elapsed = time1.max(time2);
        if elapsed < time {
            let mut new_res = res;
            for i in 0..4 { new_res[i] += robots[i] * elapsed; }
            new_res[0] -= bp.geod_ore;
            new_res[2] -= bp.geod_obs;
            robots[3] += 1;
            max = max.max(calc(bp, robots, new_res, time - elapsed, max, max_ore_need));
            robots[3] -= 1;
        }
    }
    
    max
}
