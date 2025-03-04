use aoc_lib_rust::{Day, Example, Solution};
use std::collections::HashMap;

pub struct Day20;

fn reverse_border(b: u16) -> u16 {
    b.reverse_bits() >> 6
}

struct Tile {
    id: u32,
    data: [u16; 8],
    borders: [u16; 4], // stored in order top, right, bottom, left
    counts: [u8; 4],
    rborders: [u16; 4],
    in_queue: bool,
}

impl Tile {
    fn new(id: u32, data: [u16; 8], borders: [u16; 4]) -> Self {
        Self {
            id,
            data,
            borders,
            counts: [0; 4],
            rborders: borders.map(reverse_border),
            in_queue: false,
        }
    }

    fn insert(&mut self, image: &mut [u128], x: u8, y: u8, rot: u8, flip: bool) {
        let mut data = self.data;
        for _ in 0..rot {
            let mut data_new = [0_u16; 8];
            for y in 0..8 {
                for x in 0..8 {
                    data_new[x] |= ((data[y] >> x) & 1) << (7 - y);
                }
            }
            data = data_new;
        }
        for (i, mut row) in data.into_iter().enumerate() {
            if flip {
                row = row.reverse_bits() >> 8;
            }
            image[y as usize * 8 + i] |= (row as u128) << (x * 8) as u32;
        }
    }
}

impl Day for Day20 {

    const PART1: Solution = Solution::U64(16937516456219);
    const PART2: Solution = Solution::U32(1858);

    fn solve(input: &str) -> [Solution; 2] {
        let char_to_bit = |c: u8| if c == b'.' { 0 } else { 1 };

        let mut tiles = HashMap::new();
        let mut lines = input.as_bytes().split(|c| *c == b'\n');
        while let Some(header) = lines.next() {
            let id = header[5..header.len()-1].iter()
                .fold(0, |acc, c| acc * 10 + (c - b'0') as u32);
            let mut prev_line: Option<&[u8]> = None;
            let mut borders = [0; 4];
            let mut data = [0; 8];
            let mut i = 0;
            while let Some(line) = lines.next() {
                if line.is_empty() {
                    break;
                }
                if prev_line.is_none() {
                    borders[0] = line.iter().fold(0, |acc, &c| (acc << 1) | char_to_bit(c));
                } else if i < 8 {
                    data[i] = line[1..line.len()-1].iter()
                        .fold(0, |acc, &c| (acc << 1) | char_to_bit(c));
                    i += 1;
                }
                borders[3] = (borders[3] << 1) | char_to_bit(line[0]);
                borders[1] = (borders[1] << 1) | char_to_bit(line[line.len() - 1]);
                prev_line = Some(line);
            }
            debug_assert_eq!(i, 8, "bad number of rows in image tile");
            borders[2] = prev_line.unwrap().iter()
                .fold(0, |acc, &c| (acc << 1) | char_to_bit(c));
            borders[2] = reverse_border(borders[2]);
            borders[3] = reverse_border(borders[3]);
            tiles.insert(id, Tile::new(id, data, borders));
        }

        let mut borders = HashMap::new();
        for tile in tiles.values() {
            for i in 0..4 {
                borders.entry(tile.borders[i]).or_insert(Vec::new()).push(tile.id);
                if tile.borders[i] != tile.rborders[i] {
                    borders.entry(tile.rborders[i]).or_insert(Vec::new()).push(tile.id);
                }
            }
        }

        let mut corner_ids = Vec::new();
        for tile in tiles.values_mut() {
            for i in 0..4 {
                tile.counts[i] = borders.get(&tile.borders[i]).unwrap().len() as _;
            }
            for i in 0..4 {
                if tile.counts[i] == 1 && tile.counts[(i + 1) % 4] == 1 {
                    corner_ids.push((tile.id, i as u8));
                    break;
                }
            }
        }
        debug_assert_eq!(corner_ids.len(), 4, "not all corners found");
        let sol1 = corner_ids.iter().copied().map(|(id, _)| id as u64).product();

        let mut image = Vec::new();
        let size = (tiles.len() as f32).sqrt() as usize;
        image.resize(size * 8, 0_u128);

        corner_ids.sort_unstable_by_key(|(id, _)| *id);
        let (corner_id, rot) = corner_ids[0];
        tiles.get_mut(&corner_id).unwrap().in_queue = true;

        let mut queue = vec![(corner_id, 0, size as u8 - 1, (rot + 2) % 4, true)];
        while let Some((id, x, y, rot, flip)) = queue.pop() {
            let tile = tiles.get_mut(&id).unwrap();
            tile.insert(&mut image, x, y, rot, flip);
            let tile_borders = tile.borders;
            for i in 0..4 {
                let mut trans_i = (i + 4 - rot) % 4;
                if flip && (trans_i == 1 || trans_i == 3) {
                    trans_i = (trans_i + 2) % 4;
                };
                if rot == 3 && !flip {
                    trans_i = (trans_i + 2) % 4;
                }
                let tile_border = tile_borders[trans_i as usize];

                let border_tiles = borders.get(&tile_border).unwrap();
                if border_tiles.len() != 2 {
                    continue;
                }
                let next_id = if border_tiles[0] == id {
                    border_tiles[1]
                } else {
                    border_tiles[0]
                };
                let next_tile = tiles.get_mut(&next_id).unwrap();
                if next_tile.in_queue {
                    continue;
                }

                let (x, y) = match i {
                    0 => (x, y - 1),
                    1 => (x - 1, y),
                    2 => (x, y + 1),
                    3 => (x + 1, y),
                    _ => unreachable!(),
                };
                let mut j = 0;
                let (mut rot, flip) = loop {
                    if next_tile.borders[j as usize] == tile_border {
                        break (j + 6 - i, !flip);
                    }
                    if next_tile.rborders[j as usize] == tile_border {
                        break (j + 6 - i, flip);
                    }
                    j += 1;
                };
                if flip && (i == 1 || i == 3) {
                    rot += 2;
                }
                queue.push((next_id, x, y, rot % 4, flip));
                next_tile.in_queue = true;
            }
        }

        let monster = [
            0b00000000000000000010_u128,
            0b10000110000110000111,
            0b01001001001001001000,
        ];
        let mut monster_upside_down = monster;
        monster_upside_down.reverse();
        let monsters = [
            monster,
            monster_upside_down,
            monster.map(|row| row.reverse_bits() >> 108),
            monster_upside_down.map(|row| row.reverse_bits() >> 108),
        ];

        fn count_monsters(size: usize, image: &[u128], monster: &[u128]) -> u32 {
            let mut monster_count = 0;
            for y in 0..image.len() - 2 {
                for x in 0..size * 8 {
                    let mut found = true;
                    for i in 0..3 {
                        let m = monster[i] << x;
                        if image[y + i] & m != m || m.count_ones() != monster[i].count_ones() {
                            found = false;
                            break;
                        }
                    }
                    monster_count += found as u32;
                }
            }
            monster_count
        }

        let mut image2 = image.clone();
        image2.fill(0);
        for y in 0..size*8 {
            for x in 0..size*8 {
                image2[x] |= ((image[y] >> x) & 1) << (size * 8 - 1 - y);
            }
        }

        let monster_count = monsters.iter().map(|monster| {
            count_monsters(size, &image, monster) + count_monsters(size, &image2, monster)
        }).sum::<u32>();
        let ones = image.iter().map(|row| row.count_ones()).sum::<u32>();
        let monster_ones = monsters[0].iter().map(|row| row.count_ones()).sum::<u32>();
        let sol2 = ones - monster_count * monster_ones;

        [Solution::U64(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U64(20899048083289), Solution::U32(273)],
            input: "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...",
        },
    ];
}
