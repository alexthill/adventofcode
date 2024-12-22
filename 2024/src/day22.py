from collections import deque
from utils import get_numbers

PRUNE = 16777216

class Node:
    def __init__(self, num, value):
        self.last_num = num
        self.value = value

def calc_secret(num, steps, prices):
    inital_num = num
    changes = deque()
    for i in range(0, steps):
        prev = num % 10
        num = (num ^ (num * 64)) % PRUNE
        num = (num ^ (num // 32)) % PRUNE
        num = (num ^ (num * 2048)) % PRUNE
        changes.append((num % 10) - prev)
        if len(changes) >= 4:
            if len(changes) == 5:
                changes.popleft()
            key = (changes[0], changes[1], changes[2], changes[3])
            node = prices.get(key)
            if node is None:
                prices[key] = Node(inital_num, num % 10)
            elif node.last_num != inital_num:
                node.value += num % 10
                node.last_num = inital_num
    return num

def day22(file):
    sol1 = 0
    prices = {}
    for line in file:
        if line != "\n":
            num = get_numbers(line)[0]
            sol1 += calc_secret(num, 2000, prices)

    max_node = Node(0, 0)
    for node in prices.values():
        if node.value > max_node.value:
            max_node = node
    return (sol1, max_node.value)
