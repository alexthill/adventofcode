from utils import get_digits

def find_ends(m, h, w, x, y, ends):
    elevation = m[y][x]
    if elevation == 9:
        ends[(x, y)] = True
        return 1

    count = 0
    if x > 0 and m[y][x - 1] == elevation + 1:
        count += find_ends(m, h, w, x - 1, y, ends)
    if x < w - 1 and m[y][x + 1] == elevation + 1:
        count += find_ends(m, h, w, x + 1, y, ends)
    if y > 0 and m[y - 1][x] == elevation + 1:
        count += find_ends(m, h, w, x, y - 1, ends)
    if y < h - 1 and m[y + 1][x] == elevation + 1:
        count += find_ends(m, h, w, x, y + 1, ends)
    return count

def day10(file):
    m = [get_digits(line) for line in file if line != "\n"]
    w = len(m[0])
    h = len(m)

    sol1 = 0
    sol2 = 0
    for y in range(0, h):
        for x in range(0, w):
            if m[y][x] != 0:
                continue
            ends = {}
            sol2 += find_ends(m, h, w, x, y, ends)
            sol1 += len(ends)

    return (sol1, sol2)
