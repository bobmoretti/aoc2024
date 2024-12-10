#%%
import numpy as np
import os

test_str = """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""


def get_input():
    return open("input.txt").read()


def make_map(s):

    def from_ascii(s):
        return np.array(list(s))

    return transform_coordinates(
        np.array([from_ascii(line) for line in s.splitlines()]))


DIRECTIONS = [(0, 1), (1, 0), (0, -1), (-1, 0)]
DIRECTION_MARKERS = ['^', '>', 'v', '<']


def is_off_map(pos, m):
    cy, cx = pos
    w, h = m.shape
    return cx < 0 or cy < 0 or cx >= w or cy >= h


def transform_coordinates(m):
    m = np.flip(m, axis=0)
    return m


def step(pos, direction_idx):
    y, x = pos
    dx, dy = DIRECTIONS[direction_idx]
    return y + dy, x + dx


def walk_path(m, start_pos, loop_detect=False):
    direction_idx = 0
    y, x = start_pos

    visited = set()

    while True:
        candidate = step((y, x), direction_idx)
        if is_off_map(candidate, m):
            visited.add(((y, x), direction_idx))
            return False if loop_detect else visited
        elif m[candidate] == '#':
            direction_idx += 1
            direction_idx = direction_idx % 4
        elif loop_detect and ((y, x), direction_idx) in visited:
            return True
        else:
            visited.add(((y, x), direction_idx))
            y, x = candidate


def walk_modified_path(m, cur_pos, added_obstacles, start_pos, direction):
    m2 = m.copy()
    new_obstacle_pos = step(cur_pos, direction)
    if new_obstacle_pos in added_obstacles:
        return 0
    added_obstacles.add(new_obstacle_pos)
    m2[new_obstacle_pos] = '#'
    return int(walk_path(m2, start_pos=start_pos, loop_detect=True))


def main(s):
    m = make_map(s)

    ((y, ), (x, )) = np.where(m == '^')
    start_pos = (y, x)

    def part1(s):
        visited = walk_path(m, start_pos)
        return visited

    path = part1(s)
    positions = {pos for pos, _ in path}

    print(f"part1: {len(positions)}")

    def part2(s):
        new_obs = set()
        return sum(
            walk_modified_path(m, p, new_obs, start_pos, d) for p, d in path)

    print(f"part2: {part2(s)}")


# %%
main(test_str)
# %%
%timeit
main(get_input())
# print(part1(open("input.txt").read()))
# %%
