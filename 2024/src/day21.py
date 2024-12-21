import sys

def press_button(graph, btn, curr):
    queue = [(curr, "")]
    sequences = []
    while len(sequences) == 0:
        new_queue = []
        for node, seq in queue:
            if node == btn:
                seq += "A"
                sequences.append(seq)
            for nxt, move in graph[node]:
                new_queue.append((nxt, seq + move))
        queue = new_queue
    return sequences

graph_num = {
    "A": [("0", "<"), ("3", "^")],
    "0": [("2", "^"), ("A", ">")],
    "1": [("4", "^"), ("2", ">")],
    "7": [("4", "v"), ("8", ">")],
    "9": [("8", "<"), ("6", "v")],
    "3": [("A", "v"), ("2", "<"), ("6", "^")],
    "4": [("1", "v"), ("5", ">"), ("7", "^")],
    "6": [("3", "v"), ("5", "<"), ("9", "^")],
    "8": [("5", "v"), ("7", "<"), ("9", ">")],
    "2": [("0", "v"), ("1", "<"), ("3", ">"), ("5", "^")],
    "5": [("2", "v"), ("4", "<"), ("6", ">"), ("8", "^")],
}

graph_dir = {
    "<": [("v", ">")],
    "A": [("^", "<"), (">", "v")],
    "^": [("v", "v"), ("A", ">")],
    ">": [("v", "<"), ("A", "^")],
    "v": [("<", "<"), (">", ">"), ("^", "^")],
}

def solve(code, graph, depth, cache):
    val = cache.get((code, depth))
    if val is not None:
        return val
    l = 0
    curr = "A"
    for btn in code:
        seqs = press_button(graph, btn, curr)
        min_l = sys.maxsize
        for seq in seqs:
            if depth == 0:
                min_l = min(min_l, len(seq))
            else:
                min_l = min(min_l, solve(seq, graph_dir, depth - 1, cache))
        curr = btn
        l += min_l
    cache[(code, depth)] = l
    return l

def day21(file):
    sol1 = sol2 = 0
    cache = {}
    for line in file:
        if line == "\n":
            continue
        l1 = solve(line[:len(line) - 1], graph_num, 2, cache)
        l2 = solve(line[:len(line) - 1], graph_num, 25, cache)
        sol1 += l1 * int(line[:len(line) - 2])
        sol2 += l2 * int(line[:len(line) - 2])

    return (sol1, sol2)
