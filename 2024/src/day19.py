import re

def way_count(towels, pattern, cache):
    if len(pattern) == 0:
        return 1
    if pattern in cache:
        return cache[pattern]
    ways = 0
    for i in range(1, len(pattern) + 1):
        if pattern[:i] in towels:
            ways += way_count(towels, pattern[i:], cache)
    cache[pattern] = ways
    return ways


def day19(file):
    first_line = file.readline()
    first_line = first_line[:len(first_line) - 1]
    towels = set(first_line.split(", "))
    file.readline()

    cache = {}
    sol1 = sol2 = 0
    for line in file:
        if line == "\n":
            continue
        pattern = line[:len(line) - 1]
        ways = way_count(towels, pattern, cache)
        sol1 += ways != 0
        sol2 += ways

    return (sol1, sol2)
