from utils import get_numbers

class Node:
    def __init__(self, vertices):
        self.vertices = vertices
        self.reachable = {}

def build_graph(graph, stone, n):
    if n == 0:
        return 1

    if stone in graph:
        if n in graph[stone].reachable:
            return graph[stone].reachable[n]
    elif stone == 0:
        graph[stone] = Node([1])
    else:
        s = str(stone)
        if len(s) % 2 == 0:
            a = int(s[:len(s) // 2])
            b = int(s[len(s) // 2:])
            graph[stone] = Node([a, b])
        else:
            graph[stone] = Node([stone * 2024])
    c = 0
    for sub_stone in graph[stone].vertices:
        c += build_graph(graph, sub_stone, n - 1)
    graph[stone].reachable[n] = c
    return c

def solve_part(stones, n):
    sol = 0
    graph = {}
    for stone in stones:
        sol += build_graph(graph, stone, n)
    return sol

def day11(file):
    stones = get_numbers(file.readline())
    return (solve_part(stones, 25), solve_part(stones, 75))
