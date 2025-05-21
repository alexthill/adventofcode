use aoc_lib_rust::{Day, Example, Solution};

#[derive(Debug)]
struct Cart {
    pos: i32,
    dir: [i32; 2],
    state: u8,
    crashed: bool,
}

impl Cart {
    fn new(pos: i32, dir: [i32; 2]) -> Self {
        Self { pos, dir, state: 0, crashed: false }
    }
}

pub struct Day13;

impl Day for Day13 {

    const PART1: Solution = Solution::Str("5,102");
    const PART2: Solution = Solution::Str("46,45");

    fn solve(input: &str) -> [Solution; 2] {
        let map = input.as_bytes();
        let w = map.iter().position(|c| *c == b'\n').unwrap() as i32 + 1;

        let mut carts = Vec::new();
        for (pos, &c) in map.iter().enumerate() {
            let pos = pos as i32;
            match c {
                b'>' => carts.push(Cart::new(pos, [ 1, 0])),
                b'<' => carts.push(Cart::new(pos, [-1, 0])),
                b'v' => carts.push(Cart::new(pos, [ 0, 1])),
                b'^' => carts.push(Cart::new(pos, [ 0,-1])),
                _ => {},
            }
        }

        let mut crash_count = 0;
        let mut first_crash = 0;
        while carts.len() - crash_count > 1 {
            carts.sort_unstable_by_key(|cart| cart.pos);
            for i in 0..carts.len() {
                let cart = &mut carts[i];
                if cart.crashed {
                    continue;
                }

                let c = map[cart.pos as usize];
                if c == b'/' {
                    cart.dir = [-cart.dir[1], -cart.dir[0]];
                } else if c == b'\\' {
                    cart.dir = [cart.dir[1], cart.dir[0]];
                } else if c == b'+' {
                    cart.dir = match cart.state {
                        0 => [cart.dir[1], -cart.dir[0]],
                        1 => cart.dir,
                        2 => [-cart.dir[1], cart.dir[0]],
                        _ => unreachable!(),
                    };
                    cart.state = (cart.state + 1) % 3;
                }
                let new_pos = cart.pos + cart.dir[0] + cart.dir[1] * w;

                if let Some(other) = carts.iter_mut().find(|cart| !cart.crashed && cart.pos == new_pos) {
                    if crash_count == 0 {
                        first_crash = new_pos;
                    }
                    crash_count += 2;
                    other.crashed = true;
                    carts[i].crashed = true;
                } else {
                    carts[i].pos = new_pos;
                }
            }
        }

        let sol1 = format!("{},{}", first_crash % w, first_crash / w);
        let sol2 = if let Some(last_cart) = carts.iter().find(|cart| !cart.crashed) {
             format!("{},{}", last_cart.pos % w, last_cart.pos / w)
        } else {
            String::new()
        };

        [Solution::String(sol1), Solution::String(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::Str("7,3"), Solution::None],
            input: r"/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   ",
        },
        Example {
            solution: [Solution::Str("2,0"), Solution::Str("6,4")],
            input: r"/>-<\  
|   |  
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/",
        },
    ];
}
