def search(lines, x, y, dir_x, dir_y):
    w = len(lines[0])
    h = len(lines)
    for c in "MAS":
        x += dir_x
        y += dir_y
        if not (0 <= x < w and 0 <= y < h and lines[y][x] == c):
            return False
    return True

def search2(lines, x, y, dir_x, dir_y):
    a = lines[y + dir_y][x + dir_x]
    b = lines[y - dir_y][x - dir_x]
    return a != b and (a == "M" or a == "S") and (b == "M" or b == "S")

def day04(file):
    lines = [line for line in file]
    count = 0
    for y in range(0, len(lines)):
        for x in range(0, len(lines[0])):
            if lines[y][x] == "X":
                count += search(lines, x, y, 1, 0)
                count += search(lines, x, y, -1, 0)
                count += search(lines, x, y, 0, 1)
                count += search(lines, x, y, 0, -1)
                count += search(lines, x, y, 1, 1)
                count += search(lines, x, y, -1, -1)
                count += search(lines, x, y, -1, 1)
                count += search(lines, x, y, 1, -1)

    count2 = 0
    for y in range(1, len(lines) - 1):
        for x in range(1, len(lines[0]) - 1):
            if lines[y][x] == "A":
                count2 += search2(lines, x, y, 1, 1) and search2(lines, x, y, 1, -1)

    return (count, count2)
