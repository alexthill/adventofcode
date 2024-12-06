def new_dir(m, w, h, pos, d):
    x = pos % w
    if abs(d) == 1 and (x == 0 or x == w - 1):
        return 0
    if abs(d) == w and not w <= pos < h * w - w:
        return 0
    while m[pos + d] == "#":
        if d == -w:
            d = 1
        elif d == 1:
            d = w
        elif d == w:
            d = -1
        else:
            d = -w
    return d

def day06(file):
    m = []
    w = 0
    h = 0
    for line in file:
        w = len(line) - 1
        h += 1
        m.extend(line[:w])
    
    pos, d = m.index("^"), -w
    start_pos, start_dir = pos, d
    visited = {}
    while d != 0:
        visited[pos] = True
        d = new_dir(m, w, h, pos, d)
        pos += d

    count = 0
    for place in visited.keys():
        if place == start_pos:
            continue
        m[place] = "#"
        new_visited = {}
        pos, d = start_pos, start_dir
        new_visited[(pos, d)] = True
        while d != 0:
            d = new_dir(m, w, h, pos, d)
            pos += d
            if (pos, d) in new_visited:
                count += 1
                break
            new_visited[(pos, d)] = True
        m[place] = "."

    return (len(visited), count)
