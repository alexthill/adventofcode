from utils import get_numbers

def day13(file):
    ac = 3
    bc = 1
    eps = 0.001

    sol1 = 0
    sol2 = 0
    while True:
        try:
            ax, ay = get_numbers(file.readline())
        except ValueError:
            break
        bx, by = get_numbers(file.readline())
        px, py = get_numbers(file.readline())
        file.readline()

        a = (bx * py - by * px) / (ay * bx - ax * by)
        b = (ay * px - ax * py) / (ay * bx - ax * by)
        if abs(a - round(a)) < eps and abs(b - round(b)) < eps:
            sol1 += round(a * ac + b * bc)

        px += 10000000000000
        py += 10000000000000
        a = (bx * py - by * px) / (ay * bx - ax * by)
        b = (ay * px - ax * py) / (ay * bx - ax * by)
        if abs(a - round(a)) < eps and abs(b - round(b)) < eps:
            sol2 += round(a * ac + b * bc)

    return (sol1, sol2)
