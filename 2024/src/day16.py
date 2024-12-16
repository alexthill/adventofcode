class Node:
    def __init__(self, pos, direction, dist):
        self.pos = pos
        self.direction = direction
        self.dist = dist
        self.prev = []

    def __str__(self):
        return f"Node{{ {self.pos}, {self.direction}, {self.dist} }}"

def print_map(m, w, h):
    for y in range(0, h):
        for x in range(0, w):
            print(m[y * w + x], end="")
        print("")

def day16(file):
    m = []
    w = h = 0
    for line in file:
        if line == "\n":
            break
        w = len(line) - 1
        for i, c in enumerate(line):
            if c == "\n":
                continue
            m.append(c)
        h += 1
    s = 1 + (h - 2) * w
    e = w + w - 2

    start_node = Node(s, 1, 0)
    vs = {}
    vs[(s, 1)] = start_node
    queue = {}
    queue[(s, 1)] = start_node
    while len(queue):
        min_node = None
        for node in queue.values():
            if min_node is None or node.dist < min_node.dist:
                min_node = node
        del queue[(min_node.pos, min_node.direction)]

        for d in [1, -1, w, -w]:
            pos = min_node.pos + d
            if m[pos] == "#":
                continue
            cost = min_node.dist + (1 if abs(d) == abs(min_node.direction) else 1001)
            node = vs.get((pos, d))
            if node is None:
                node = Node(pos, d, cost)
                node.prev = [min_node]
                vs[(pos, d)] = node
                queue[(pos, d)] = node
            elif node.dist > cost:
                node.dist = cost
                node.prev = [min_node]
                queue[(pos, d)] = node
            elif node.dist == cost:
                node.prev.append(min_node)

    min_node = None
    for d in [1, -1, w, -w]:
        node = vs.get((e, d))
        if min_node is None or (node is not None and node.dist < min_node.dist):
            min_node = node

    queue = set([min_node])
    visited = set()
    while len(queue):
        node = queue.pop()
        visited.add(node.pos)
        for prev in node.prev:
            queue.add(prev)

    return (min_node.dist, len(visited))
