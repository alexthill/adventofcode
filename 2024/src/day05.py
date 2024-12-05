from utils import get_numbers

def must_be_before(a, b, ordering):
    return a in ordering and b in ordering[a]

def sort_pages(pages, ordering):
    swap = True
    while swap:
        swap = False
        for i in range(1, len(pages)):
            if must_be_before(pages[i], pages[i - 1], ordering):
                pages[i - 1], pages[i] = pages[i], pages[i - 1]
                swap = True

def check_ordering(pages, ordering):
    for i, page in enumerate(pages):
        if page in ordering:
            for page2 in ordering[page]:
                if page2 in pages[:i]:
                    return False
    return True

def day05(file):
    ordering = {}
    in_part1 = True
    sol1 = 0
    sol2 = 0
    for line in file:
        if in_part1:
            if len(line) <= 1:
                in_part1 = False
            else:
                a, b = get_numbers(line)
                if a not in ordering:
                    ordering[a] = []
                ordering[a].append(b)
        else:
            pages = get_numbers(line)
            if check_ordering(pages, ordering):
                sol1 += pages[len(pages) // 2]
            else:
                sort_pages(pages, ordering)
                sol2 += pages[len(pages) // 2]
    return (sol1, sol2)
