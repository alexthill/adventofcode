"""
The solution for part 2 does not work for the example input.
It may even only work for my input as I have done some amount of reverse engineering.

Reverse engineered program:
    2,4, -> B = A % 8
    1,2, -> B = B XOR 2
    7,5, -> C = A >> B (0 <= B < 8)
    0,3, -> A = A / 8
    4,7, -> B = B XOR C
    1,7, -> B = B XOR 7
    5,5, -> out B
    3,0  -> jnz 0

I found that low digits of A (in base 8) only affect the first part of the output.
Therefore you can find the inital A by first brute forcing the highest digit
and then the second highest and so on.
"""

from utils import get_numbers

def combo(ra, rb, rc, lit):
    match lit:
        case 0:
            return 0
        case 1:
            return 1
        case 2:
            return 2
        case 3:
            return 3
        case 4:
            return ra
        case 5:
            return rb
        case 6:
            return rc
        case _:
            print("bad combo literal")
            return -1

def run_prog(ra, rb, rc, prog):
    out = []
    ic = 0
    while ic < len(prog) - 1:
        op = prog[ic]
        lit = prog[ic + 1]
        match op:
            case 0:
                comb = combo(ra, rb, rc, lit)
                ra = int(ra / (2**comb))
            case 1:
                rb = rb ^ lit
            case 2:
                comb = combo(ra, rb, rc, lit)
                rb = comb % 8
            case 3:
                if ra != 0:
                    ic = lit - 2
            case 4:
                rb = rb ^ rc
            case 5:
                comb = combo(ra, rb, rc, lit)
                out.append(comb % 8)
            case 6:
                comb = combo(ra, rb, rc, lit)
                rb = int(ra / (2**comb))
            case 7:
                comb = combo(ra, rb, rc, lit)
                rc = int(ra / (2**comb))
            case _:
                print("unknown op")
        ic += 2
    return out

def day17(file):
    ra = get_numbers(file.readline())[0]
    rb = get_numbers(file.readline())[0]
    rc = get_numbers(file.readline())[0]
    file.readline()
    prog = get_numbers(file.readline())
    states = set()

    out = run_prog(ra, rb, rc, prog)
    sol1 = ",".join([str(n) for n in out])

    sol2 = 8**(len(prog) - 1)
    for e in reversed(range(0, len(prog))):
        for i in range(0, 8):
            ra = sol2 + 8**e * i
            out = run_prog(ra, rb, rc, prog)
            print(out)
            if out[e] == prog[e]:
            
                sol2 = ra
                break
    out = run_prog(sol2, rb, rc, prog)

    return (sol1, sol2)
