use aoc_lib_rust::{Day, Example, Solution};
use std::collections::HashMap;
use std::fmt;

pub struct Day20;

fn reverse_border(b: u16) -> u16 {
    b.reverse_bits() >> 6
}

#[derive(Debug)]
struct Tile {
    id: u32,
    // stored in order top, right, bottom, left
    borders: [u16; 4],
    counts: [u8; 4],
    rborders: [u16; 4],
    rcounts: [u8; 4],
}

impl Tile {
    fn new(id: u32, borders: [u16; 4]) -> Self {
        Self {
            id,
            borders,
            counts: [0; 4],
            rborders: borders.map(reverse_border),
            rcounts: [0; 4],
        }
    }
}

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Tile {{ {}:\n\t[", self.id)?;
        for (i, border) in self.borders.iter().enumerate() {
            write!(f, "{border:010b} {:2}, ", self.counts[i])?;
        }
        write!(f, "]\n\t[")?;
        for (i, rborder) in self.rborders.iter().enumerate() {
            write!(f, "{rborder:010b} {:2}, ", self.rcounts[i])?;
        }
        write!(f, "]\n}}")
    }
}

impl Day for Day20 {

    const PART1: Solution = Solution::U64(16937516456219);
    const PART2: Solution = Solution::None;

    fn solve(input: &str) -> [Solution; 2] {
        let char_to_bit = |c: u8| if c == b'.' { 0 } else { 1 };

        let mut tiles = Vec::new();
        let mut lines = input.as_bytes().split(|c| *c == b'\n');
        while let Some(header) = lines.next() {
            let id = header[5..header.len()-1].iter()
                .fold(0, |acc, c| acc * 10 + (c - b'0') as u32);
            let mut prev_line: Option<&[u8]> = None;
            let mut borders = [0; 4];
            while let Some(line) = lines.next() {
                if line.is_empty() {
                    break;
                }
                if prev_line.is_none() {
                    borders[0] = line.iter().fold(0, |acc, &c| (acc << 1) | char_to_bit(c));
                }
                borders[3] = (borders[3] << 1) | char_to_bit(line[0]);
                borders[1] = (borders[1] << 1) | char_to_bit(line[line.len() - 1]);
                prev_line = Some(line);
            }
            borders[2] = prev_line.unwrap().iter()
                .fold(0, |acc, &c| (acc << 1) | char_to_bit(c));
            borders[2] = reverse_border(borders[2]);
            borders[3] = reverse_border(borders[3]);
            tiles.push(Tile::new(id, borders));
        }

        let mut borders = HashMap::new();
        for tile in tiles.iter() {
            for i in 0..4 {
                *borders.entry(tile.borders[i]).or_insert(0) += 1;
                if tile.borders[i] != tile.rborders[i] {
                    *borders.entry(tile.rborders[i]).or_insert(0) += 1;
                }
            }
        }

        let mut sol1 = 1;
        for tile in tiles.iter_mut() {
            for i in 0..4 {
                tile.counts[i] = borders.get(&tile.borders[i]).copied().unwrap();
                tile.rcounts[i] = borders.get(&tile.rborders[i]).copied().unwrap();
            }
            println!("{tile}");
            for i in 0..4 {
                if tile.counts[i] == 1 && tile.counts[(i + 1) % 4] == 1 {
                    sol1 *= tile.id as u64;
                    break;
                }
            }
        }

        [Solution::U64(sol1), Solution::None]
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
