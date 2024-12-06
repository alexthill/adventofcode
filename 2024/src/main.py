#!/bin/python3

import sys

from day01 import day01
from day02 import day02
from day03 import day03
from day04 import day04
from day05 import day05
from day06 import day06

def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} [day]")
        return

    day = int(sys.argv[1])
    file_prefix = "../inputs/"
    print(f"solving day {day} ...")
    match day:
        case 1:
            with open(file_prefix + "day01.txt") as file:
                part1, part2 = day01(file)
        case 2:
            with open(file_prefix + "day02.txt") as file:
                part1, part2 = day02(file)
        case 3:
            with open(file_prefix + "day03.txt") as file:
                part1, part2 = day03(file)
        case 4:
            with open(file_prefix + "day04.txt") as file:
                part1, part2 = day04(file)
        case 5:
            with open(file_prefix + "day05.txt") as file:
                part1, part2 = day05(file)
        case 6:
            with open(file_prefix + "day06.txt") as file:
                part1, part2 = day06(file)
        case _:
            print(f"no solution for day {day}")
            return
    print(f"part 1: {part1}")
    print(f"part 2: {part2}")

if __name__ == "__main__":
    main()
