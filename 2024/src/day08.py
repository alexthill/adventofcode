def day08(file):
    nodes_by_freq = {}
    w = 0
    h = 0
    for y, line in enumerate(file):
        h += 1
        w = len(line) - 1
        for x, c in enumerate(line):
            if c == "." or c == "\n" or c == "#":
                continue
            if c not in nodes_by_freq:
                nodes_by_freq[c] = []
            nodes_by_freq[c].append((x, y))

    locs1 = {}
    locs2 = {}
    for nodes in nodes_by_freq.values():
        for a in nodes:
            for b in nodes:
                if a == b:
                    continue
                ax, ay = a
                bx, by = b
                dx, dy = ax - bx, ay - by
                lx, ly = ax + dx, ay + dy

                if 0 <= lx < w and 0 <= ly < h:
                    locs1[(lx, ly)] = True

                locs2[(ax, ay)] = True
                while 0 <= lx < w and 0 <= ly < h:
                    locs2[(lx, ly)] = True
                    lx, ly = lx + dx, ly + dy

    return (len(locs1), len(locs2))
