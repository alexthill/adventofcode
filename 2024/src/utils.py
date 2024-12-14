def get_numbers(s):
    numbers = []
    num = None
    for c in s:
        if c >= "0" and c <= "9":
            if num is None:
                num = int(c) 
            else:
                num = num * 10 + int(c)
        elif num is not None:
            numbers.append(num)
            num = None
    if num is not None:
        numbers.append(num)
    return numbers

def get_numbers_signed(s):
    numbers = []
    num = None
    sign = 1
    for c in s:
        if c == "-":
            sign = -1
        if c >= "0" and c <= "9":
            if num is None:
                num = int(c) 
            else:
                num = num * 10 + int(c)
        elif num is not None:
            numbers.append(num * sign)
            num = None
            sign = 1
    if num is not None:
        numbers.append(num * sign)
    return numbers

def get_digits(s):
    return [int(c) for c in s if c.isdigit()]
