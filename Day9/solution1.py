"""My solution for Day 9!"""

TAIL = [0, 0]
HEAD = [0, 0]
TAIL_LOCATIONS = set()


def move_up(n, index):
    if index >= n:
        return True
    HEAD[1] += 1
    if not TAIL[1] + 1 >= HEAD[1]:
        TAIL[1] += 1
        TAIL[0] = HEAD[0]
    TAIL_LOCATIONS.add(tuple(TAIL.copy()))
    move_up(n, index + 1)

def move_down(n, index):
    if index >= n:
        return True
    HEAD[1] -= 1
    if not TAIL[1] - 1 <= HEAD[1]:
        TAIL[1] -= 1
        TAIL[0] = HEAD[0]
    TAIL_LOCATIONS.add(tuple(TAIL.copy()))
    move_down(n, index + 1)

def move_left(n, index):
    if index >= n:
        return True
    HEAD[0] -= 1
    if not TAIL[0] - 1 <= HEAD[0]:
        TAIL[0] -= 1
        TAIL[1] = HEAD[1]
    TAIL_LOCATIONS.add(tuple(TAIL.copy()))
    move_left(n, index + 1)

def move_right(n, index):
    if index >= n:
        return True
    HEAD[0] += 1
    if not TAIL[0] + 1 >= HEAD[0]:
        TAIL[0] += 1
        TAIL[1] = HEAD[1]
    TAIL_LOCATIONS.add(tuple(TAIL.copy()))
    move_right(n, index + 1)


def main():
    with open("source.txt", "r") as INPUT:
        for line in INPUT:
            instructions = line.split(" ")
            match instructions[0]:
                case "U":
                    move_up(int(instructions[1]), 0)
                case "D":
                    move_down(int(instructions[1]), 0)
                case "L":
                    move_left(int(instructions[1]), 0)
                case "R":
                    move_right(int(instructions[1]), 0)
        print (len(TAIL_LOCATIONS))


if __name__ == "__main__":
    main()
