def calc_sol(m, w, h, box_char):
    sol = 0
    for y in range(0, h):
        for x in range(0, w):
            if m[y * w + x] == box_char:
                sol += x + 100 * y
    return sol

def print_map(m, w, h):
    for y in range(0, h):
        for x in range(0, w):
            print(m[y * w + x], end="")
        print("")

def day15(file):
    m, m2, dirs = [], [], []
    w = h = p = p2 = 0
    for line in file:
        if line == "\n":
            break
        w = len(line) - 1
        for i, c in enumerate(line):
            if c == "\n":
                continue
            m.append(c)
            if c == "@":
                p = w * h + i
                p2 = w * 2 * h + i * 2
                m2.append("@")
                m2.append(".")
            elif c == "O":
                m2.append("[")
                m2.append("]")
            else:
                m2.append(c)
                m2.append(c)
        h += 1
    for line in file:
        for c in line:
            if c == "<":
                dirs.append((-1, 0))
            elif c == ">":
                dirs.append((1, 0))
            elif c == "^":
                dirs.append((0, -1))
            elif c == "v":
                dirs.append((0, 1))

    for dx, dy in dirs:
        d = dy * w + dx
        end = p + d
        while m[end] == "O":
            end += d
        if m[end] == "#":
            continue
        while m[end] != "@":
            m[end] = m[end - d]
            end -= d
        m[end] = "."
        p += d
    sol1 = calc_sol(m, w, h, "O")

    m = m2
    p = p2
    w *= 2
    for dx, dy in dirs:
        d = dy * w + dx
        end = p + d
        if abs(d) == 1:
            while m[end] == "[" or m[end] == "]":
                end += d
            if m[end] == "#":
                continue
            while m[end] != "@":
                m[end] = m[end - d]
                end -= d
            m[end] = "."
            p += d
        else:
            visited = set()
            queue = [p]
            while len(queue):
                a = queue.pop()
                if a in visited or m[a] == ".":
                    continue
                visited.add(a)
                if m[a] == "#":
                    break
                queue.append(a + d)
                if m[a] == "[":
                    queue.append(a + 1)
                elif m[a] == "]":
                    queue.append(a - 1)
            else:
                visited = list(visited)
                visited.sort()
                if d > 0:
                    visited.reverse()
                for a in visited:
                    m[a + d] = m[a]
                    m[a] = "."
                p += d
    sol2 = calc_sol(m, w, h, "[")

    return (sol1, sol2)
