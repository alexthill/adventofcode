from utils import get_numbers

def day01(file):
    l1 = []
    l2 = []
    for line in file:
        a, b = get_numbers(line)
        l1.append(a)
        l2.append(b)
    l1.sort()
    l2.sort()
    total_dist = 0
    for a, b in zip(l1, l2):
        total_dist += abs(a - b)

    l2_counts = {}
    num = -1
    count = 0
    for a in l2:
        if a == num:
            count += 1
        else:
            if num != -1:
                l2_counts[num] = count
            num = a
            count = 1
    l2_counts[num] = count

    similarity = 0
    for a in l1:
        if a in l2_counts:
            similarity += a * l2_counts[a];
    return (total_dist, similarity)
