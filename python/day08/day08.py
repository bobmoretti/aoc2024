#%%
import numpy as np
from itertools import combinations, chain

test_str = """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"""


def print_ascii(m):
    for line in m:
        print(''.join(line))


def get_input():
    return open("input.txt").read()


def make_map(s):

    def from_ascii(s):
        return np.array(list(s))

    locs = np.array([from_ascii(line) for line in s.splitlines()])
    return locs


def find_all_antinodes(locs, m, r=(1, 2)):
    h, w = m.shape

    def is_in_bounds(pos):
        x, y = pos
        return 0 <= x < w and 0 <= y < h

    def find_nodes(p1, p2):
        delta = np.array(p2 - p1)
        units = np.arange(*r)
        deltas = delta * units[:, np.newaxis]
        nodes = []
        nodes.extend(p1 - deltas)
        nodes.extend(p2 + deltas)

        return [tuple(n) for n in nodes if is_in_bounds(n)]

    antenna_pairs = combinations(locs, 2)
    return chain.from_iterable(
        [find_nodes(np.array(p1), np.array(p2)) for p1, p2 in antenna_pairs])


def search(m, r=(1, 2)):
    antennas = [ch for ch in np.unique(m) if ch != '.']
    nodes = set()
    for antenna in antennas:
        x, y = np.where(m == antenna)
        locs = np.array([x, y]).T
        locs = set([tuple(l) for l in locs])
        possible_nodes = find_all_antinodes(locs, m, r=r)
        nodes |= set(possible_nodes)
    return nodes


def part1(s):
    m = make_map(s)
    return len(search(m))


def part2(s):
    m = make_map(s)
    return len(search(m, (0, 51)))


# %%
print(part1(test_str))
print(part1(open("input.txt").read()))
print(part2(test_str))
print(part2(open("input.txt").read()))

# %%
m = make_map(test_str)
nodes = search(m, 50)
# %%
nodes
# %%
m[np.array(list(nodes))]
# %%
m
# %%

# %%
s_result = """##....#....#
.#.#....0...
..#.#0....#.
..##...0....
....0....#..
.#...#A....#
...#..#.....
#....#.#....
..#.....A...
....#....A..
.#........#.
...#......##"""
m2 = m.copy()
m2[*np.array(list(nodes)).T] = '#'
# print(m)

print(make_map(s_result))

print(m2)

# %%
