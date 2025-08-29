use aoc_lib_rust::{next, Day, Example, Solution};
use aoc_lib_rust::utils::vector::Vector;

type IVec3 = Vector<i32, 3>;

#[derive(Debug, Clone, Copy)]
struct Particle {
    acc: IVec3,
    vel: IVec3,
    pos: IVec3,
    id: u32,
}

pub struct Day20;

impl Day for Day20 {

    const PART1: Solution = Solution::U32(457);
    const PART2: Solution = Solution::U32(448);

    fn solve(input: &str) -> [Solution; 2] {
        let mut particles = input.lines().enumerate().map(|(id, line)| {
            let mut parts = line.split(['<', '>', ',']);
            assert_eq!(next!(parts), "p=");
            let pos = IVec3::from(std::array::from_fn(|_| next!(parts).trim().parse().unwrap()));
            assert_eq!(next!(parts), "");
            assert_eq!(next!(parts), " v=");
            let vel = IVec3::from(std::array::from_fn(|_| next!(parts).trim().parse().unwrap()));
            assert_eq!(next!(parts), "");
            assert_eq!(next!(parts), " a=");
            let acc = IVec3::from(std::array::from_fn(|_| next!(parts).trim().parse().unwrap()));
            Particle { pos, vel, acc, id: id as _ }
        }).collect::<Vec<_>>();

        let sol1 = particles.iter().min_by(|a, b| {
            let acc_a = a.acc.x().abs() + a.acc.y().abs() + a.acc.z().abs();
            let vel_a = a.vel.x().abs() + a.vel.y().abs() + a.vel.z().abs();
            let pos_a = a.pos.x().abs() + a.pos.y().abs() + a.pos.z().abs();
            let acc_b = b.acc.x().abs() + b.acc.y().abs() + b.acc.z().abs();
            let vel_b = b.vel.x().abs() + b.vel.y().abs() + b.vel.z().abs();
            let pos_b = b.pos.x().abs() + b.pos.y().abs() + b.pos.z().abs();
            acc_a.cmp(&acc_b).then(vel_a.cmp(&vel_b)).then(pos_a.cmp(&pos_b))
        }).unwrap().id;

        for _ in 0..50 {
            for particle in particles.iter_mut() {
                particle.vel += particle.acc;
                particle.pos += particle.vel;
            }
            particles.sort_unstable_by_key(|particle| particle.pos);
            let mut insert_idx = 0;
            let mut i = 0;
            while i < particles.len() {
                let j = i;
                i += 1;
                while i < particles.len() && particles[i].pos == particles[j].pos {
                    i += 1;
                }
                if i - j == 1 {
                    particles[insert_idx] = particles[j];
                    insert_idx += 1;
                }
            }
            particles.truncate(insert_idx);
        }
        let sol2 = particles.len() as u32;

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(0), Solution::None],
            input: "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>",
        },
        Example {
            solution: [Solution::None, Solution::U32(1)],
            input: "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>
p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>
p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>",
        },
    ];
}
