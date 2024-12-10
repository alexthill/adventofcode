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

def main():
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} [day] [flags]")
        print(f"list of flags:")
        print(f"-e\t\trun example input")
        return

    flags = {}
    for flag in sys.argv[2:]:
        flags[flag] = True

    day = int(sys.argv[1])
    zero = "0" if day < 10 else ""
    example = "_example" if "-e" in flags else ""
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
        case _:
            print(f"no solution for day {day}")
            return
    duration = round(time.time() - start_time, 3)
    print(f"... took {duration} seconds")
    print(f"> part 1: {part1}")
    print(f"> part 2: {part2}")

if __name__ == "__main__":
    main()
