def day09(file):
    line = file.read()

    disk = []
    free = False
    free_spans = []
    full_spans = []
    for i, d in enumerate(line):
        if not d.isdigit():
            continue
        if free:
            free_spans.append((len(disk), int(d)))
        else:
            full_spans.append((len(disk), int(d), i // 2))
        for _ in range(0, int(d)):
            disk.append(-1 if free else i // 2)
        free = not free
    disk2 = disk.copy()

    i = 0
    j = len(disk) - 1
    while True:
        while disk[j] == -1:
            j -= 1
        while disk[i] != -1:
            i += 1
        if i >= j:
            break
        disk[i] = disk[j]
        disk[j] = -1

    sol1 = sum(k * v for k, v in enumerate(disk) if v != -1)

    disk = disk2
    next_free_pos_by_size = [0 for _ in range(10)]
    for full_pos, full_size, full_id in reversed(full_spans):
        if full_pos < next_free_pos_by_size[1]:
            break
        for i in range(next_free_pos_by_size[full_size], len(free_spans)):
            free_pos, free_size = free_spans[i]
            if free_pos > full_pos:
                break;
            if free_size >= full_size:
                free_spans[i] = (free_pos + full_size, free_size - full_size)
                next_free_pos_by_size[full_size] = i
                for j in range(0, full_size):
                    disk[free_pos + j] = full_id
                for j in range(0, full_size):
                    disk[full_pos + j] = -1
                break

    sol2 = sum(k * v for k, v in enumerate(disk) if v != -1)

    return (sol1, sol2)
