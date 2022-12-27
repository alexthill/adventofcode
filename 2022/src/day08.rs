use crate::Solution;

pub const SOLUTION: Solution = Solution::U32((1533, 345744));

pub fn solve(input: String) -> Solution {
    let input = input.as_bytes();
    let map_w = input.iter().position(|&c| c == b'\n').unwrap();
    assert_eq!(input.len() % (map_w + 1), 0);
    let map = input.chunks_exact(map_w + 1).collect::<Vec<_>>();
    let map_h = map.len();
    
    let mut vis_map = Vec::new();
    vis_map.resize(map_h * map_w, false);
    
    for i in 0..map_h {
        let mut max_l = b'\0';
        for j in 0..map_w {
            if map[i][j] > max_l {
                vis_map[i * map_w + j] = true;
                max_l = map[i][j];
                if max_l == b'9' {
                    break;
                }
            }
        }
        
        let mut max_r = b'\0';
        for j in (0..map_w).rev() {
            if map[i][j] > max_r {
                vis_map[i * map_w + j] = true;
                if map[i][j] == max_l {
                    break;
                } else {
                    max_r = map[i][j];
                }
            }
        }
    }
    
    for j in 0..map_w {
        let mut max_t = b'\0';
        for i in 0..map_h {
            if map[i][j] > max_t {
                vis_map[i * map_w + j] = true;
                max_t = map[i][j];
                if max_t == b'9' {
                    break;
                }
            }
        }
        
        let mut max_b = b'\0';
        for i in (0..map_h).rev() {
            if map[i][j] > max_b {
                vis_map[i * map_w + j] = true;
                if map[i][j] == max_t {
                    break;
                } else {
                    max_b = map[i][j];
                }
            }
        }
    }
    
    let vis_count = vis_map.iter().fold(0, |acc, &vis| acc + vis as u32);
    
    let mut best_scenic = 0;
    for i in 1..map_h - 1 {        
        for j in 1..map_w - 1 {
            let h = map[i][j];
            let max_l = j;
            let max_r = map_w - j - 1;
            let max_t = i;
            let max_b = map_h - i - 1;
            
            if h == b'0' || max_l * max_r * max_t * max_b <= best_scenic {
                continue;
            }
            
            let mut score_l = 1;
            while score_l < max_l && map[i][j - score_l] < h {
                score_l += 1;
            }
            
            if score_l * max_r * max_t * max_b <= best_scenic {
                continue;
            }
            
            let mut score_r = 1;
            while score_r < max_r && map[i][j + score_r] < h {
                score_r += 1;
            }
            
            if score_l * score_r * max_t * max_b <= best_scenic {
                continue;
            }
            
            let mut score_t = 1;
            while score_t < max_t && map[i - score_t][j] < h {
                score_t += 1;
            }
            
            if score_l * score_r * score_t * max_b <= best_scenic {
                continue;
            }
            
            let mut score_b = 1;
            while score_b < max_b && map[i + score_b][j] < h {
                score_b += 1;
            }
            
            let score = score_l * score_r * score_t * score_b;
            if score > best_scenic {
                best_scenic = score;
            }
        }
    }

    (vis_count, best_scenic as u32).into()
}
