def day25(file):
    locks = []
    keys = []
    while True:
        line = file.readline()
        if len(line) == 0:
            break
        if line == "\n":
            continue
        is_key = line == ".....\n"
        item = [0, 0, 0, 0, 0]
        for i in range(0, 5):
            line = file.readline()
            for j in range(0, 5):
                item[j] += line[j] == "#"
        file.readline()
        if is_key:
            keys.append(item)
        else:
            locks.append(item)

    sol1 = 0
    for lock in locks:
        for key in keys:
            for i in range(0, 5):
                if lock[i] + key[i] > 5:
                    break
            else:
                sol1 += 1

    return (sol1, 0)
