def remove_from_edges(edges, x, y, dx, dy, a, b):
    try:
        while True:
            x += a
            y += b
            edges.remove((x, y, dx, dy))
    except KeyError:
        pass

def fill(m, x, y):
    w = len(m[0])
    h = len(m)
    c = m[y][x]
    a = 0
    p = 0
    queue = [(x, y)]
    edges = set()
    while len(queue):
        x, y = queue.pop()
        if m[y][x] == c.lower():
            continue
        m[y][x] = c.lower()
        a += 1
        p += 4
        for dx, dy in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            if 0 <= y + dy < h and 0 <= x + dx < w:
                if m[y + dy][x + dx] == c:
                    queue.append((x + dx, y + dy))
                    p -= 2
                elif m[y + dy][x + dx] != c.lower():
                    edges.add((x, y, dx, dy))
            else:
                edges.add((x, y, dx, dy))

    sides = 0
    while len(edges):
        x, y, dx, dy = edges.pop()
        sides += 1
        if dx != 0:
            remove_from_edges(edges, x, y, dx, dy, 0, 1)
            remove_from_edges(edges, x, y, dx, dy, 0, -1)
        else:
            remove_from_edges(edges, x, y, dx, dy, 1, 0)
            remove_from_edges(edges, x, y, dx, dy, -1, 0)

    return (a, p, sides)
        
def day12(file):
    m = []
    for line in file:
        if line == "\n":
            continue
        m.append([c for c in line if c != "\n"])

    sol1 = 0
    sol2 = 0
    for y in range(0, len(m)):
        for x in range(0, len(m[0])):
            if m[y][x].isupper():
                a, p, sides = fill(m, x, y)
                sol1 += a * p
                sol2 += a * sides
    return (sol1, sol2)
