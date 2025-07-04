use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::{next_parse, utils::vector::Vector};

type IVec2 = Vector<i32, 2>;
type IVec3 = Vector<i32, 3>;

fn overlap(a: IVec2, b: IVec2) -> Option<IVec2> {
    let [a_min, a_max] = a.into();
    let [b_min, b_max] = b.into();
    if a_max < b_min || b_max < a_min {
        None
    } else {
        Some([a_min.max(b_min), a_max.min(b_max)].into())
    }
}

#[derive(Debug, Clone)]
struct Range {
    count: u32,
    pruned: bool,
    split_dim: u8,
    split: i32,
    halves: Option<Box<[Range; 2]>>,
}
impl Range {
    fn new(count: u32) -> Self {
        Range { count, pruned: false, split_dim: 0, split: 0, halves: None }
    }

    fn intersection(a: [IVec2; 4], b: [IVec2; 4]) -> Option<[IVec2; 4]> {
        let Some(o0) = overlap(a[0], b[0]) else { return None };
        let Some(o1) = overlap(a[1], b[1]) else { return None };
        let Some(o2) = overlap(a[2], b[2]) else { return None };
        let Some(o3) = overlap(a[3], b[3]) else { return None };
        Some([o0, o1, o2, o3])
    }

    fn add_range(
        &mut self,
        coords: [IVec2; 4],
        other: [IVec2; 4],
        count: u32,
        max_count: u32,
    ) -> bool {
        if self.pruned {
            return true;
        }
        let Some(inter) = Self::intersection(coords, other) else {
            return false;
        };
        if coords == inter {
            self.count += 1;
            return false;
        }

        if self.halves.is_none() {
            // A magic number here, that is probably bigger than the maximum
            // number of bots that are part of the maximum cover.
            if max_count - count > 30 {
                return true;
            }
            let (dim, _) = coords.iter().enumerate()
                .find(|(dim, range)| **range != inter[*dim])
                .unwrap();
            self.split_dim = dim as _;
            self.split = if inter[dim].x() == coords[dim].x() {
                inter[dim].y()
            } else {
                inter[dim].x() - 1
            };
            self.halves = Some(Box::new([Range::new(0), Range::new(0)]));
        }

        let [l_coords, r_coords] = self.child_coords(coords);
        let halves = self.halves.as_mut().unwrap();
        let count = count + self.count;
        let l_prune = halves[0].add_range(l_coords, inter, count, max_count);
        let r_prune = halves[1].add_range(r_coords, inter, count, max_count);
        if l_prune && r_prune {
            self.halves = None;
            self.pruned = true;
            true
        } else {
            false
        }
    }

    fn child_coords(&self, coords: [IVec2; 4]) -> [[IVec2; 4]; 2] {
        let mut l_coords = coords;
        *l_coords[self.split_dim as usize].y_mut() = self.split;
        let mut r_coords = coords;
        *r_coords[self.split_dim as usize].x_mut() = self.split + 1;
        [l_coords, r_coords]
    }

    fn best_overlap(
        &self,
        coords: [IVec2; 4],
        count: u32,
        best_cover: &mut u32,
        best_coords: &mut [IVec2; 4],
        best_man: &mut i32,
    ) {
        let count = count + self.count;
        if let Some(halves) = self.halves.as_ref() {
            let [l_coords, r_coords] = self.child_coords(coords);
            halves[0].best_overlap(l_coords, count, best_cover, best_coords, best_man);
            halves[1].best_overlap(r_coords, count, best_cover, best_coords, best_man);
        } else if count >= *best_cover {
            let man = coords.map(|range| {
                range.x().abs().min(range.y().abs())
            }).into_iter().sum::<i32>();
            if count > *best_cover || man < *best_man {
                *best_cover = count;
                *best_coords = coords;
                *best_man = man;
            }
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

        // Coordinate change from 3D to 4D in such a way that the diamond
        // ranges become axis aligned boxes.
        let boxes = bots.iter().copied().map(|(pos, r)| {
            let (x, y, z) = (pos.x(), pos.y(), pos.z());
            let (x, y, z, w) = (x + y + z, x + y - z, x - y + z, -x + y + z);
            [[x - r, x + r], [y - r, y + r], [z - r, z + r], [w - r, w + r]]
        }).collect::<Vec<_>>();
        let bounding = boxes.iter().fold([[i32::MAX, i32::MIN]; 4], |acc, b| [
           [acc[0][0].min(b[0][0]), acc[0][1].max(b[0][1])],
           [acc[1][0].min(b[1][0]), acc[1][1].max(b[1][1])],
           [acc[2][0].min(b[2][0]), acc[2][1].max(b[2][1])],
           [acc[3][0].min(b[3][0]), acc[3][1].max(b[3][1])],
        ]).map(IVec2::from);

        let mut range = Range::new(0);
        for (i, b) in boxes.into_iter().enumerate() {
            range.add_range(bounding, b.map(IVec2::from), 0, i as _);
        }
        let mut cover = 0;
        let mut coords = Default::default();
        let mut man = 0;
        range.best_overlap(bounding, 0, &mut cover, &mut coords, &mut man);

        let count_cover = |coords: IVec3| {
            bots.iter().filter(|(pos, r)| {
                (coords - *pos).map(|x| x.abs()).sum() <= *r
            }).count()
        };

        // We know have some box in 4D space. Either there is a bug or I do not
        // understand how this works but just taking the minimum manhattan dist
        // of this box is wrong.
        // So we need to check all points in this box if they have the best
        // cover or not.
        let mut min_man = i32::MAX;
        for v in coords[1].x()..=coords[1].y() {
            for w in coords[2].x()..=coords[2].y() {
                for u in coords[3].x()..=coords[3].y() {
                    let point = IVec3::from([(v + w) / 2, (v + u) / 2, (u + w) / 2]);
                    if count_cover(point) != cover as usize {
                        continue;
                    }
                    min_man = min_man.min(point.map(|x| x.abs()).sum());
                }
            }
        }
        let sol2 = min_man;

        // Another working solution that tries to find the best corner of
        // all ranges and then searches from there to find the solution.
        // Produces the correct minimum manhattan distance but for the
        // wrong point. I do not understand why.
        // The first phase is fast but then searching around for a better
        // solution takes some time.
        /*
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
        let mut i = 1_i32;
        'outer: loop {
            let mut found = false;
            for dx in -i..=i {
                for dy in -i..=i  {
                    for dz in -i..=i {
                        let corner = best.1 + IVec3::from([dx, dy, dz]);
                        let count = count_cover(corner);
                        if count < best.0 {
                            continue;
                        }

                        let manhattan = corner.map(|x| x.abs()).sum();
                        if count > best.0 {
                            best = (count, corner, manhattan);
                            println!("{i} {best:?}");
                            i = 1.max(i / 2);
                            continue 'outer;
                        }
                        if manhattan < best.2 {
                            best = (count, corner, manhattan);
                            found = true;
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
        */

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
