import re

from utils import get_numbers

def day03(file):
    res1, res2 = 0, 0
    enabled = True
    for find in re.findall("mul\(\d+,\d+\)|do\(\)|don't\(\)", file.read()):
        if find[0] == "m":
            a, b = get_numbers(find)
            res1 += a * b
            if enabled:
                res2 += a * b
        else:
            enabled = find[2] == "("
    return (res1, res2)
