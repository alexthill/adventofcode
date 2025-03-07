#!/bin/python3

import sys
import time

from day01 import day01
from day02 import day02
from day03 import day03
from day04 import day04
from day05 import day05
from day06 import day06
from day07 import day07
from day08 import day08
from day09 import day09
from day10 import day10
from day11 import day11
from day12 import day12
from day13 import day13
from day14 import day14
from day15 import day15
from day16 import day16
from day17 import day17
from day18 import day18
from day19 import day19
from day20 import day20
from day21 import day21
from day22 import day22
from day23 import day23
from day24 import day24
from day25 import day25

def main():
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} [day] [flags]")
        print(f"list of flags:")
        print(f"-e\t\trun example input")
        return

    flags = {}
    for flag in sys.argv[2:]:
        flags[flag] = True
    is_example = "-e" in flags

    day = int(sys.argv[1])
    zero = "0" if day < 10 else ""
    example = "_example" if is_example else ""
    file_name = f"../inputs/day{zero}{day}{example}.txt"
    try:
        file = open(file_name, "r")
    except:
        print(f"failed to open file '{file_name}'")
        return

    print(f"solving day {day} ...")
    start_time = time.time()
    match day:
        case 1: part1, part2 = day01(file)
        case 2: part1, part2 = day02(file)
        case 3: part1, part2 = day03(file)
        case 4: part1, part2 = day04(file)
        case 5: part1, part2 = day05(file)
        case 6: part1, part2 = day06(file)
        case 7: part1, part2 = day07(file)
        case 8: part1, part2 = day08(file)
        case 9: part1, part2 = day09(file)
        case 10: part1, part2 = day10(file)
        case 11: part1, part2 = day11(file)
        case 12: part1, part2 = day12(file)
        case 13: part1, part2 = day13(file)
        case 14: part1, part2 = day14(file, is_example)
        case 15: part1, part2 = day15(file)
        case 16: part1, part2 = day16(file)
        case 17: part1, part2 = day17(file)
        case 18: part1, part2 = day18(file, is_example)
        case 19: part1, part2 = day19(file)
        case 20: part1, part2 = day20(file, is_example)
        case 21: part1, part2 = day21(file)
        case 22: part1, part2 = day22(file)
        case 23: part1, part2 = day23(file)
        case 24: part1, part2 = day24(file)
        case 25: part1, part2 = day25(file)
        case _:
            print(f"no solution for day {day}")
            return
    duration = round(time.time() - start_time, 3)
    print(f"... took {duration} seconds")
    print(f"> part 1: {part1}")
    print(f"> part 2: {part2}")

if __name__ == "__main__":
    main()
