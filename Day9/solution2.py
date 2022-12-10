"""My solution for Day 9!"""
import time


TAIL = [[0, 0] for i in range(9)]
HEAD = [0, 0]
TAIL_LOCATIONS = set()

def check_distance(tail, head):
    if abs(head[1] - tail[1]) > 1:
        if head[1] - tail[1] > 1:
            # head is up
            tail[1] += 1
        else:
            # head is down
            tail[1] -= 1
        tail[0] = head[0]
        check_distance(tail, head)
    elif abs(head[0] - tail[0]) > 1:
        if head[0] - tail[0] > 1:
            # head is to the right
            tail[0] += 1
        else:
            # head is to the left
            tail[0] -= 1
        tail[1] = head[1]
        check_distance(tail, head)
    else:
        TAIL_LOCATIONS.add(tuple(TAIL[8]))
        return True


def move_up(n, index):
    if index >= n:
        return True
    HEAD[1] += 1
    for i, tail in enumerate(TAIL):
        if i == 0:
            check_distance(tail, HEAD)
        else:
            check_distance(tail, TAIL[i-1])
    move_up(n, index + 1)

def move_down(n, index):
    if index >= n:
        return True
    HEAD[1] -= 1
    for i, tail in enumerate(TAIL):
        if i == 0:
            check_distance(tail, HEAD)
        else:
            check_distance(tail, TAIL[i-1])
    move_down(n, index + 1)

def move_left(n, index):
    if index >= n:
        return True
    HEAD[0] -= 1
    for i, tail in enumerate(TAIL):
        if i == 0:
            check_distance(tail, HEAD)
        else:
            check_distance(tail, TAIL[i-1])
    move_left(n, index + 1)

def move_right(n, index):
    if index >= n:
        return True
    HEAD[0] += 1
    for i, tail in enumerate(TAIL):
        if i == 0:
            check_distance(tail, HEAD)
        else:
            check_distance(tail, TAIL[i-1])
    move_right(n, index + 1)

def display(input, size):
    disp = ""
    for i in range(-int(size / 2) - HEAD[1], int(size / 2) - HEAD[1]):
        disp += "\n"
        for j in range(-int(size / 2) + HEAD[0], int(size / 2) + HEAD[0]):
            if [j, -i] == HEAD:
                disp += "\u001b[34mO\u001b[37m "
            elif [j, -i] in input:
                disp += "\u001b[36mX\u001b[37m "
            elif tuple([j, -i]) in TAIL_LOCATIONS:
                disp += "\u001b[33m#\u001b[37m "
            else:
                disp += ".\u001b[37m "
    print (disp)

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
            time.sleep(1/24)
            display(TAIL, 45)
            print(line, len(TAIL_LOCATIONS))
        print (len(TAIL_LOCATIONS))


if __name__ == "__main__":
    main()
