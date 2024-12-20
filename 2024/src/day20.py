def find_cheats(m, w, h, max_dur, min_dist):
    cheats = {}
    count = 0
    for cheat_start in range(w, h * w - w):
        sx = cheat_start % w
        sy = cheat_start // h
        if m[cheat_start] == -1:
            continue
        for cheat_end in range(w, h * w - w):
            if cheat_end == cheat_start or m[cheat_end] == -1:
                continue
            ex = cheat_end % w
            ey = cheat_end // h
            d = abs(sx - ex) + abs(sy - ey)
            shortcut = m[cheat_end] - m[cheat_start] - d
            if d <= max_dur and shortcut > 0 and shortcut >= min_dist:
                count += 1
                if not shortcut in cheats:
                    cheats[shortcut] = 0
                cheats[shortcut] += 1
    return cheats, count

def day20(file, is_example):
    m = []
    w = h = s = e = 0
    for line in file:
        if line == "\n":
            continue
        w = len(line) - 1
        for i, c in enumerate(line[:w]):
            m.append(-1 if c == "#" else 0)
            if c == "S":
                s = h * w + i
            elif c == "E":
                e = h * w + i
        h += 1

    p = s
    i = 1
    while p != e:
        m[p] = i
        for d in [1, -1, w, -w]:
            if m[p + d] == 0:
                i += 1
                p += d
                break
    m[p] = i

    min_dist = 0 if is_example else 100
    sol1 = 0
    for y in range(1, h - 1):
        for x in range(1, w - 1):
            if m[x + y * w] == -1:
                for d in [1, w]:
                    a = m[x + y * w + d]
                    b = m[x + y * w - d]
                    if a != -1 and b != -1:
                        shortcut = abs(a - b) - 2
                        if shortcut >= min_dist:
                            sol1 += 1

    min_dist = 50 if is_example else 100
    cheats, sol2 = find_cheats(m, w, h, 20, min_dist)

    return (sol1, sol2)
