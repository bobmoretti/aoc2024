#%%
import numpy as np
from functools import reduce

test_str = """89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"""

cartesian_directions = np.array([[-1, 0], [1, 0], [0, -1], [0, 1]])


def parse(s):
    g = np.array([[int(x) for x in line] for line in s.splitlines()])
    return np.pad(g, 1, constant_values=100)


def find_valid_neighbors(grid, position):
    neighbors = position + cartesian_directions
    deltas = grid[*neighbors.T] - grid[*position]
    valids = neighbors[np.where(deltas == 1)]
    final_positions, = np.where(grid[*valids.T] == 9)
    return valids, final_positions


def find_valid_summits(grid, position):
    valids, final_positions = find_valid_neighbors(grid, position)
    found_peaks = set(map(tuple, valids[final_positions]))
    return found_peaks | reduce(set.union, (find_valid_summits(grid, pos)
                                            for pos in valids), set())


def count_valid_paths(grid, position):
    valids, final_positions = find_valid_neighbors(grid, position)
    return len(final_positions) + sum(
        count_valid_paths(grid, pos) for pos in valids)


def part1(s):
    grid = parse(s)
    trailheads = np.array(np.where(grid == 0)).T
    return sum(len(find_valid_summits(grid, t)) for t in trailheads)


def part2(s):
    grid = parse(s)
    trailheads = np.array(np.where(grid == 0)).T
    return sum(count_valid_paths(grid, t) for t in trailheads)


# %%
print(part1(test_str))
print(part1(open("input.txt").read()))
print(part2(test_str))
print(part2(open("input.txt").read()))

# %%
