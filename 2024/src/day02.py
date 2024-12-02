from utils import get_numbers

def is_save(nums):
    prev = nums[0]
    increasing = None
    for num in nums[1:]:
        if num == prev or abs(num - prev) > 3:
            return False
        if increasing is None:
            increasing = num > prev
        elif increasing != (num > prev):
            return False
        prev = num
    return True

def day02(file):
    save_count = 0
    save_count2 = 0

    for line in file:
        nums = get_numbers(line)
        if len(nums) < 2:
            save_count += 1
            continue;

        if is_save(nums):
            save_count += 1

        for i in range(0, len(nums)):
            copy = nums.copy()
            del copy[i]
            if is_save(copy):
                save_count2 += 1
                break

    return (save_count, save_count2)
