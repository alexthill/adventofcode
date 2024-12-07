from utils import get_numbers

def check(values, i, res):
    if res > values[0]:
        return False
    if i == len(values):
        return res == values[0]
    return check(values, i + 1, res + values[i])\
        or check(values, i + 1, res * values[i])

def check2(values, i, res):
    if res > values[0]:
        return False
    if i == len(values):
        return res == values[0]
    return check2(values, i + 1, res + values[i])\
        or check2(values, i + 1, res * values[i])\
        or check2(values, i + 1, int(str(res) + str(values[i])))

def day07(file):
    sol1 = 0
    sol2 = 0
    for line in file:
        nums = get_numbers(line)
        if check(nums, 2, nums[1]):
            sol1 += nums[0]
        if check2(nums, 2, nums[1]):
            sol2 += nums[0]
    return (sol1, sol2)
