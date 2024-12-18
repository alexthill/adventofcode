from utils import get_numbers

def find_path(m, w, h):
    queue = set([(0, 0)])
    visited = {}
    visited[(0, 0)] = None
    while len(queue):
        new_queue = set()
        for x, y in queue:
            if x == w - 1 and y == h - 1:
                path = set()
                node = (x, y)
                while node:
                    path.add(node)
                    node = visited[node]
                return path
            for dx, dy in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
                px, py = x + dx, y + dy
                if not (0 <= px < w and 0 <= py < h and m[y][x] == 0) or (px, py) in visited:
                    continue
                new_queue.add((px, py))
                visited[(px, py)] = (x, y)
        queue = new_queue
    return None

def day18(file, example):
    bs = []
    for line in file:
        if line != "\n":
            bs.append(get_numbers(line))

    if example:
        w = h = 7
        steps = 12
    else:
        w = h = 71
        steps = 1024
    m = [[0 for i in range(0, w)] for j in range(0, h)]

    for i in range(0, steps):
        x, y = bs[i]
        m[y][x] = 1

    path = find_path(m, w, h)
    new_path = path

    for i in range(steps, len(bs)):
        x, y = bs[i]
        m[y][x] = 1
        if (x, y) in new_path:
            new_path = find_path(m, w, h)
            if new_path is None:
                break

    return (len(path) - 1, bs[i])
