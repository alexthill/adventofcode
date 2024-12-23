class Node:
    def __init__(self, name):
        self.name = name
        self.children = set()

def find_max_cluster(nodes, node):
    for name_del in node.children.copy():
        node.children.remove(name_del)
        for child_name in node.children:
            inter = node.children.intersection(nodes[child_name].children)
            if len(inter) != len(node.children) - 1:
                break
        else:
            cluster = list(node.children)
            cluster.append(node.name)
            cluster.sort()
            return cluster
        node.children.add(name_del)
    return None

def day23(file):
    nodes = {}
    for line in file:
        if line == "\n":
            continue
        name_a, name_b = line[:len(line) - 1].split("-")

        node = nodes.get(name_a)
        if node is None:
            node = Node(name_a)
            nodes[name_a] = node
        node.children.add(name_b)

        node = nodes.get(name_b)
        if node is None:
            node = Node(name_b)
            nodes[name_b] = node
        node.children.add(name_a)

    groups = set()
    for node1 in nodes.values():
        if node1.name[0] != "t":
            continue
        for name2 in node1.children:
            node2 = nodes[name2]
            for name3 in node2.children:
                if name3 in node1.children:
                    group = [node1.name, name2, name3]
                    group.sort()
                    group = (group[0], group[1], group[2])
                    if group not in groups:
                        groups.add(group)

    max_cluster = None
    for node in nodes.values():
        max_cluster = find_max_cluster(nodes, node)
        if max_cluster is not None:
            break

    return (len(groups), ",".join(max_cluster))
