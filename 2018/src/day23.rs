use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::{next_parse, utils::vector::Vector};

type IVec3 = Vector<i32, 3>;

fn overlap(a: (i32, i32), b: (i32, i32)) -> Option<(i32, i32)> {
    let (a_min, a_max) = a;
    let (b_min, b_max) = b;
    if a_max < b_min || b_max < a_min {
        None
    } else {
        Some((a_min.max(b_min), a_max.min(b_max)))
    }
}

#[derive(Debug, Clone)]
struct Range {
    count: u32,
    split_dim: u8,
    split: i32,
    halves: Option<Box<[Range; 2]>>,
}
impl Range {
    fn new(count: u32) -> Self {
        Range { count, split_dim: 0, split: 0, halves: None }
    }

    fn intersection(coords: &[(i32, i32); 4], other: &[(i32, i32); 4]) -> Option<[(i32, i32); 4]> {
        let (a, b) = (coords, other);
        let Some(o0) = overlap(a[0], b[0]) else { return None };
        let Some(o1) = overlap(a[1], b[1]) else { return None };
        let Some(o2) = overlap(a[2], b[2]) else { return None };
        let Some(o3) = overlap(a[3], b[3]) else { return None };
        Some([o0, o1, o2, o3])
    }

    fn add_range(
        &mut self,
        coords: [(i32, i32); 4],
        other: [(i32, i32); 4],
        count: u32,
        max_count: u32,
    ) {
        debug_assert!(coords.iter().all(|(start, end)| start <= end));
        debug_assert!(other.iter().all(|(start, end)| start <= end));

        let Some(inter) = Self::intersection(&coords, &other) else { return };
        if coords == inter {
            self.count += 1;
            return;
        }
        if self.halves.is_none() {
            if max_count - count > 50 {
                return;
            }
            let (dim, _) = coords.iter().enumerate()
                .filter(|(dim, range)| **range != inter[*dim])
                .next().unwrap();
            self.split_dim = dim as _;
            self.split = if inter[dim].0 == coords[dim].0 {
                inter[dim].1
            } else {
                inter[dim].0 - 1
            };
            self.halves = Some(Box::new([Range::new(0), Range::new(0)]));
        }
        let [l_coords, r_coords] = self.child_coords(coords);

        debug_assert!(l_coords.iter().all(|(start, end)| start <= end));
        debug_assert!(r_coords.iter().all(|(start, end)| start <= end));

        if let Some(halves) = self.halves.as_mut() {
            let count = count + self.count;
            halves[0].add_range(l_coords, inter, count, max_count);
            halves[1].add_range(r_coords, inter, count, max_count);
        } else {
            unreachable!();
        }
    }

    fn child_coords(&self, coords: [(i32, i32); 4]) -> [[(i32, i32); 4]; 2] {
        let mut l_coords = coords;
        l_coords[self.split_dim as usize].1 = self.split;
        let mut r_coords = coords;
        r_coords[self.split_dim as usize].0 = self.split + 1;
        [l_coords, r_coords]
    }

    fn best_overlap(&self, coords: [(i32, i32); 4], count: u32) -> (u32, i32, [(i32, i32); 4]) {
        let count = count + self.count;
        if let Some(halves) = self.halves.as_ref() {
            let [l_coords, r_coords] = self.child_coords(coords);
            [
                halves[0].best_overlap(l_coords, count),
                halves[1].best_overlap(r_coords, count),
            ].into_iter().max_by(|a, b| {
                a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1).reverse())
            }).unwrap()
        } else {
            let min_dist = coords.map(|(min, max)| {
                if min < 0 && max > 0 {
                    0
                } else {
                    min.abs().min(max.abs())
                }
            }).into_iter().sum::<i32>();
            (count, min_dist, coords)
        }
    }
}

pub struct Day23;

impl Day for Day23 {

    const PART1: Solution = Solution::U32(510);
    const PART2: Solution = Solution::U32(108889300);

    fn solve(input: &str) -> [Solution; 2] {
        let bots = input.lines().map(|line| {
            let (pos, r) = line.split_once(' ').unwrap();
            let mut pos = pos["pos=<".len()..pos.len() - 2].split(',');
            let pos = IVec3::from([
                next_parse!(pos, i32),
                next_parse!(pos, i32),
                next_parse!(pos, i32),
            ]);
            let r = r[2..].parse::<i32>().unwrap();
            (pos, r)
        }).collect::<Vec<_>>();

        let (max_pos, max_r) = bots.iter().max_by_key(|(_, r)| r).unwrap();
        let sol1 = bots.iter().filter(|(pos, _)| {
            (*max_pos - *pos).map(|x| x.abs()).sum() <= *max_r
        }).count();


        /*
        let boxes = bots.into_iter().map(|(pos, r)| {
            let (x, y, z) = (pos.x(), pos.y(), pos.z());
            let (x, y, z, w) = (x + y + z, x + y - z, x - y + z, -x + y + z);
            [(x - r, x + r), (y - r, y + r), (z - r, z + r), (w - r, w + r)]
        }).collect::<Vec<_>>();
        let bounding = boxes.iter().fold([(i32::MAX, i32::MIN); 4], |acc, b| [
           (acc[0].0.min(b[0].0), acc[0].1.max(b[0].1)),
           (acc[1].0.min(b[1].0), acc[1].1.max(b[1].1)),
           (acc[2].0.min(b[2].0), acc[2].1.max(b[2].1)),
           (acc[3].0.min(b[3].0), acc[3].1.max(b[3].1)),
        ]);
        println!("bounding {bounding:?}");

        let mut range = Range::new(0);
        for (i, b) in boxes.into_iter().enumerate() {
            println!(">{i} {b:?}");
            range.add_range(bounding, b, 0, i as _);
        }
        //println!("{range:?}");
        let best = range.best_overlap(bounding, 0);
        println!("{best:?}");
        let sol2 = best.1 / 2;
        */

        /*
        let mut inters = boxes.iter().enumerate().map(|(i, b)| (i, *b, vec![i])).collect::<Vec<_>>();
        let mut inters_next = Vec::new();
        for i in 1..boxes.len() {
            println!("round {i} {}", inters.len());
            for (start, a, set) in inters.iter() {
                for (box_idx, b) in boxes.iter().enumerate().skip(start + 1) {
                    let Some(o0) = overlap(a[0], b[0]) else { continue };
                    let Some(o1) = overlap(a[1], b[1]) else { continue };
                    let Some(o2) = overlap(a[2], b[2]) else { continue };
                    let Some(o3) = overlap(a[3], b[3]) else { continue };
                    let inter = [o0, o1, o2, o3];
                    let mut set = set.clone();
                    set.push(box_idx);
                    inters_next.push((box_idx, inter, set));
                }
            }
            if inters_next.len() == 0 {
                break;
            }
            std::mem::swap(&mut inters, &mut inters_next);
            inters_next.clear();
        }
        println!("{} {:?}", inters.len(), inters.iter().map(|(_, _, set)| set.clone()).collect::<Vec<_>>());
        let sol2 = inters[0].1.map(|(min, max)| {
            if min < 0 && max > 0 {
                0
            } else {
                min.abs().min(max.abs())
            }
        }).into_iter().sum::<i32>() / 2;
        */

        /*
        fn max_overlap(
            d: u32,
            boxes: &[[(i32, i32); 4]],
            inter: [(i32, i32); 4],
            mut idx: usize,
        ) -> (u32, [(i32, i32); 4]) {
            let mut best = (d, inter);
            for b in &boxes[idx..] {
                idx += 1;
                let a = inter;
                let Some(o0) = overlap(a[0], b[0]) else { continue };
                let Some(o1) = overlap(a[1], b[1]) else { continue };
                let Some(o2) = overlap(a[2], b[2]) else { continue };
                let Some(o3) = overlap(a[3], b[3]) else { continue };
                let inter = max_overlap(d + 1, boxes, [o0, o1, o2, o3], idx);
                if inter.0 > best.0 {
                    best = inter;
                }
            }
            best
        }
        let (count, inter) = max_overlap(0, &boxes, [(i32::MIN, i32::MAX); 4], 0);
        println!("{boxes:?}");
        println!("{count} {inter:?}");
        let sol2 = inter.map(|(min, max)| {
            if min < 0 && max > 0 {
                0
            } else {
                min.abs().min(max.abs())
            }
        }).into_iter().sum::<i32>() / 2;
        */

        /*
        let inters = boxes.iter().copied().enumerate().flat_map(|(i, a)| {
            boxes.iter().copied().enumerate().filter_map(move |(j, b)| {
                if i == j { return None; }
                let Some(o0) = overlap(a[0], b[0]) else { return None };
                let Some(o1) = overlap(a[1], b[1]) else { return None };
                let Some(o2) = overlap(a[2], b[2]) else { return None };
                let Some(o3) = overlap(a[3], b[3]) else { return None };
                Some([o0, o1, o2, o3])
            })
        }).collect::<Vec<_>>();
        println!("{inters:?}");
        */

        let count_cover = |coords: IVec3| {
            bots.iter().filter(|(pos, r)| {
                (coords - *pos).map(|x| x.abs()).sum() <= *r
            }).count()
        };

        let mut best = (0, IVec3::default(), 0);
        for (pos, r) in bots.iter().copied() {
            for dx in -1..=1_i32 {
                for dy in -1..=1_i32 {
                    for dz in -1..=1_i32 {
                        if dx.abs() + dy.abs() + dz.abs() != 1 {
                            continue;
                        }
                        let corner = pos + IVec3::from([dx, dy, dz]) * r;
                        let count = count_cover(corner);
                        if count >= best.0 {
                            let manhattan = corner.map(|x| x.abs()).sum();
                            if count > best.0 || manhattan < best.2 {
                                best = (count, corner, manhattan);
                            }
                        }
                    }
                }
            }
        }
        let mut i = 1;
        loop {
            let mut found = false;
            for dx in -i..=i {
                for dy in -i..=i  {
                    for dz in -i..=i {
                        let corner = best.1 + IVec3::from([dx, dy, dz]);
                        let count = count_cover(corner);
                        if count > best.0 {
                            let manhattan = corner.map(|x| x.abs()).sum();
                            best = (count, corner, manhattan);
                            println!("{i} {best:?}");
                            found = true;
                        } else if count == best.0 {
                            let manhattan = corner.map(|x| x.abs()).sum();
                            if manhattan < best.2 {
                                best = (count, corner, manhattan);
                                println!("{i} {best:?}");
                                found = true;
                            }
                        }
                    }
                }
            }
            if !found {
                break;
            }
            i += 1;
        }
        let sol2 = best.1.map(|x| x.abs()).sum();

        [Solution::U32(sol1 as _), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(7), Solution::None],
            input: "pos=<0,0,0>, r=4
pos=<1,0,0>, r=1
pos=<4,0,0>, r=3
pos=<0,2,0>, r=1
pos=<0,5,0>, r=3
pos=<0,0,3>, r=1
pos=<1,1,1>, r=1
pos=<1,1,2>, r=1
pos=<1,3,1>, r=1",
        },
        Example {
            solution: [Solution::U32(6), Solution::U32(36)],
            input: "pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5",
        },
    ];
}
