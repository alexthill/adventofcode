from utils import get_numbers_signed

class Robot:
    def __init__(self, nums):
        self.x = nums[0]
        self.y = nums[1]
        self.vx = nums[2]
        self.vy = nums[3]

    def move(self, w, h, m):
        m[self.y][self.x] -= 1
        self.x = (self.x + self.vx + w) % w
        self.y = (self.y + self.vy + h) % h
        m[self.y][self.x] += 1

    def __str__(self):
        return f"Robot{{x: {self.x}, y: {self.y}, vx: {self.vx}, vy: {self.vy}}}"

def generate_map(w, h, robots):
    m = [[0 for i in range(0, w)] for j in range(0, h)]
    for robot in robots:
        m[robot.y][robot.x] += 1
    return m

def print_map(m, w, h):
    for y in range(0, h):
        for x in range(0, w):
            c = m[y][x]
            if c == 0:
                print(" ", end="")
            else:
                print(str(c), end="")
        print("")

def biggest_cluster(m, w, h):
    m = [row[:] for row in m]
    biggest = 0
    for y in range(0, h):
        for x in range(0, w):
            if m[y][x] == 0:
                continue
            count = 0
            queue = [(x, y)]
            while len(queue):
                x, y = queue.pop()
                if m[y][x] == 0:
                    continue
                m[y][x] = 0
                count += 1
                for dx, dy in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
                    if 0 <= x + dx < w and 0 <= y + dy < h:
                        queue.append((x + dx, y + dy))
            biggest = max(biggest, count)
    return biggest


def day14(file, example):
    if example:
        w, h = 11, 7
    else:
        w, h = 101, 103
    robots = []
    for line in file:
        if line == "\n":
            continue
        robots.append(Robot(get_numbers_signed(line)))
    m = generate_map(w, h, robots)

    for i in range(0, 100):
        for robot in robots:
            robot.move(w, h, m)
    quadrants = [0, 0, 0, 0]
    for robot in robots:
        if robot.x < w // 2 and robot.y < h // 2:
            quadrants[0] += 1
        elif robot.x > w // 2 and robot.y < h // 2:
            quadrants[1] += 1
        elif robot.x < w // 2 and robot.y > h // 2:
            quadrants[2] += 1
        elif robot.x > w // 2 and robot.y > h // 2:
            quadrants[3] += 1
    sol1 = quadrants[0] * quadrants[1] * quadrants[2] * quadrants[3]

    if example:
        sol2 = None
    else:
        max_biggest = 0
        for i in range(0, 1000000):
            for robot in robots:
                robot.move(w, h, m)
            biggest = biggest_cluster(m, w, h)
            max_biggest = max(biggest, max_biggest)
            if biggest > len(robots) // 4:
                #print_map(m, w, h)
                break
        sol2 = i + 101

    return (sol1, sol2)
