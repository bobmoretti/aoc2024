# %%
import numpy as np
from scipy import signal as sig

test_str = """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""


def make_diagonal(kernel, inc):
    a, b = inc
    x = y = 3
    for ii in range(4):
        kernel[x, y] = from_ascii("XMAS")[ii]
        x += a
        y += b


def make_kernels_part1():
    kernels = []
    kernels = [np.zeros((7, 7), np.int32) for _ in range(8)]
    kernels[0][3, :4] = from_ascii("SAMX")
    kernels[1][3, 3:] = from_ascii("XMAS")
    kernels[2][:4, 3] = from_ascii("SAMX")
    kernels[3][3:, 3] = from_ascii("XMAS")
    make_diagonal(kernels[4], (-1, -1))
    make_diagonal(kernels[5], (-1, 1))
    make_diagonal(kernels[6], (1, -1))
    make_diagonal(kernels[7], (1, 1))
    return kernels


def make_kernels_part2():
    kernels = [
        [from_ascii("M.S"),
         from_ascii(".A."),
         from_ascii("M.S")],
        [from_ascii("S.S"),
         from_ascii(".A."),
         from_ascii("M.M")],
        [from_ascii("S.M"),
         from_ascii(".A."),
         from_ascii("S.M")],
        [from_ascii("M.M"),
         from_ascii(".A."),
         from_ascii("S.S")],
    ]
    return kernels


def from_ascii(s):
    s = s.replace('.', '\x00')
    return np.array(list(s)).view(np.uint32)


def get_input():
    return open("input.txt").readlines()


def to_matrix(lines):
    lines = np.array([list(line.replace('.', '\x00')) for line in lines])
    lines = lines.view(np.uint32)
    return lines


def count_correlations(m, kernel, bingo):
    return np.sum(sig.correlate(m, kernel, mode='same') == bingo)


def find_matches(lines, kernels, bingo):
    m = to_matrix(lines)
    return sum(count_correlations(m, k, bingo) for k in kernels)


def part1(lines):
    bingo = np.dot(from_ascii("XMAS"), from_ascii("XMAS"))
    return find_matches(lines, make_kernels_part1(), bingo)


def part2(lines):
    kernels = make_kernels_part2()
    bingo = sig.correlate2d(kernels[0], kernels[0], mode='same')[1,1]
    return find_matches(lines, kernels, bingo)


print(part1(test_str.splitlines()))
print(part1(get_input()))
print(part2(test_str.splitlines()))
print(part2(get_input()))

# %%
