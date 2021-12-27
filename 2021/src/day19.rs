use std::fs;
use std::collections::HashSet;

#[derive(Debug, PartialEq, Clone)]
struct Scanner {
    id: u32,
    pos: Option<(usize, (i32, i32, i32))>,
    beacons: HashSet<(i32, i32, i32)>,
}
impl Scanner {
    fn from_str(id: u32, string: &str) -> Self {
        let beacons = string.lines().map(|line| {
            let mut parts = line.split(',');
            (parts.next().unwrap().parse::<i32>().unwrap(), parts.next().unwrap().parse::<i32>().unwrap(), parts.next().unwrap().parse::<i32>().unwrap())
        }).collect::<HashSet<(i32, i32, i32)>>();
        Self { id, pos: None, beacons }
    }
    
    fn find_overlap(&self, ref_scanner: &Scanner, rot_mats: &[Matrix]) -> (u32, (usize, (i32, i32, i32))) {
        let mut max_overlap: u32 = 0;
        let mut best_pos = (0, (0, 0, 0));
        for (mat_id, &mat) in rot_mats.iter().enumerate() {
            for mut self_beacon in self.beacons.iter().copied() {
                self_beacon = matrix_vec_mul(mat, self_beacon);
                for ref_beacon in ref_scanner.beacons.iter() {
                    let self_pos = (ref_beacon.0 - self_beacon.0, ref_beacon.1 - self_beacon.1, ref_beacon.2 - self_beacon.2);
                    let mut overlap = 0;
                    for mut beacon in self.beacons.iter().copied() {
                        beacon = matrix_vec_mul(mat, beacon);
                        let beacon_pos = (beacon.0 + self_pos.0, beacon.1 + self_pos.1, beacon.2 + self_pos.2);
                        if ref_scanner.beacons.contains(&beacon_pos) {
                            overlap += 1;
                        }
                    }
                    
                    if overlap > max_overlap {
                        max_overlap = overlap;
                        best_pos = (mat_id, self_pos);
                    }
                }
            }
        }
        (max_overlap, best_pos)
    }
    
    fn apply_transformation(&mut self, trans: (usize, (i32, i32, i32)), rot_mats: &[Matrix]) {
        self.pos = Some(trans);
        let mut old_beacons = HashSet::new();
        std::mem::swap(&mut self.beacons, &mut old_beacons);
        
        for beacon in old_beacons.drain() {
            let new_beacon = matrix_vec_mul(rot_mats[trans.0], beacon);
            self.beacons.insert((new_beacon.0 + trans.1.0, new_beacon.1 + trans.1.1, new_beacon.2 + trans.1.2));
        }
    }
}

pub fn solve() {
    let input = fs::read_to_string("inputs/19.txt").unwrap();
    let mut scanners = input.split("--- scanner").skip(1).enumerate().map(|(i, string)| {
        Scanner::from_str(i as u32, string.split("---").nth(1).unwrap().trim())
    }).collect::<Vec<Scanner>>();
    let mats = get_rot_matrices();
    scanners[0].pos = Some((0, (0, 0, 0)));
    let mut undet_scanners = (1..scanners.len()).collect::<HashSet<usize>>();
    let mut det_scanners = vec![0];
    while let Some(ref_scanner) = det_scanners.pop() {
        let mut new_undet_scanners = HashSet::new();
        for scanner in undet_scanners.drain() {
            let res = scanners[scanner].find_overlap(&scanners[ref_scanner], &mats);
            if res.0 < 12 {
                new_undet_scanners.insert(scanner);
            } else {
                scanners[scanner].apply_transformation(res.1, &mats);
                det_scanners.push(scanner);
            }
        }
        if new_undet_scanners.is_empty() {
            break;
        }
        undet_scanners = new_undet_scanners;
    }
    if !undet_scanners.is_empty() {
        panic!("could not determine all scanner positions");
    }
    
    let mut all_beacons = HashSet::new();
    for scanner in scanners.iter() {
        for beacon in scanner.beacons.iter() {
            all_beacons.insert(beacon);
        }
    }
    println!("beacon count: {}", all_beacons.len());
    
    let mut max_dist = 0;
    for (i, scanner1) in scanners.iter().enumerate() {
        for scanner2 in scanners.iter().skip(i+1) {
            let pos1 = scanner1.pos.unwrap();
            let pos2 = scanner2.pos.unwrap();
            let dist = (pos1.1.0 - pos2.1.0).abs()
                + (pos1.1.1 - pos2.1.1).abs()
                + (pos1.1.2 - pos2.1.2).abs();
            if dist > max_dist {
                max_dist = dist;
            }
        }
    }
    println!("max dist: {}", max_dist);
}

type Matrix = ((i32, i32, i32), (i32, i32, i32), (i32, i32, i32));

const MATRICES: [Matrix; 4] = [
    (
        ( 1, 0, 0),
        ( 0, 1, 0),
        ( 0, 0, 1),
    ),
    // around z
    (
        ( 0, 1, 0),
        (-1, 0, 0),
        ( 0, 0, 1),
    ),
    // around x
    (
        ( 1, 0, 0),
        ( 0, 0, 1),
        ( 0,-1, 0),
    ),
    // around y
    (
        ( 0, 0, 1),
        ( 0, 1, 0),
        (-1, 0, 0),
    ),
];

fn matrix_mul(a: Matrix, b: Matrix) -> Matrix {
    (
        (a.0.0 * b.0.0 + a.0.1 * b.1.0 + a.0.2 * b.2.0, a.0.0 * b.0.1 + a.0.1 * b.1.1 + a.0.2 * b.2.1, a.0.0 * b.0.2 + a.0.1 * b.1.2 + a.0.2 * b.2.2),
        (a.1.0 * b.0.0 + a.1.1 * b.1.0 + a.1.2 * b.2.0, a.1.0 * b.0.1 + a.1.1 * b.1.1 + a.1.2 * b.2.1, a.1.0 * b.0.2 + a.1.1 * b.1.2 + a.1.2 * b.2.2),
        (a.2.0 * b.0.0 + a.2.1 * b.1.0 + a.2.2 * b.2.0, a.2.0 * b.0.1 + a.2.1 * b.1.1 + a.2.2 * b.2.1, a.2.0 * b.0.2 + a.2.1 * b.1.2 + a.2.2 * b.2.2),
    )
}

fn matrix_vec_mul(a: Matrix, b: (i32, i32, i32)) -> (i32, i32, i32) {
    (
        a.0.0 * b.0 + a.0.1 * b.1 + a.0.2 * b.2,
        a.1.0 * b.0 + a.1.1 * b.1 + a.1.2 * b.2,
        a.2.0 * b.0 + a.2.1 * b.1 + a.2.2 * b.2,
    )
}

fn get_rot_matrices() -> Box::<[Matrix]> {
    let mut mats = Vec::new();
    for i in 0..4 {
        for j in 0..4 {
            for k in 0..4 {
                for l in 0..4 {
                    let mat = matrix_mul(matrix_mul(matrix_mul(MATRICES[i], MATRICES[j]), MATRICES[k]), MATRICES[l]);
                    if !mats.contains(&mat) {
                        mats.push(mat);
                    }
                }
            }
        }
    }
    mats.into_boxed_slice()
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_rot_matrices() {
        let mats = get_rot_matrices();
        assert_eq!(mats.len(), 24);
        assert_eq!(mats[0], MATRICES[0]);
        assert_eq!(mats[1], MATRICES[1]);
        assert_eq!(mats[2], MATRICES[2]);
        assert_eq!(mats[3], MATRICES[3]);
    }
    
    #[test]
    fn test_matrix_mul() {
        let res_mat = (
            (-1, 0, 0),
            ( 0,-1, 0),
            ( 0, 0, 1),
        );
        assert_eq!(matrix_mul(MATRICES[1], MATRICES[1]), res_mat);
    }

    // #[test]
    // fn create_scanner() {
        // let scanner = Scanner::from_str(0, r#"-1,-1,1
// -2,-2,2
// -2,-2,2
// -2,-3,1
// 5,6,-4
// 8,0,7
// "#);
        // assert_eq!(scanner, Scanner {
            // id: 0,
            // pos: None,
            // beacons: vec![(-1,-1,1), (-2,-2,2), (-2,-2,2), (-2,-3,1), (5,6,-4), (8,0,7)],
        // });
    // }
}
