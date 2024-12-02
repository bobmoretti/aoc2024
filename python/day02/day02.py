# %%
import numpy as np

test_str = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9""".splitlines()


def parselines(fname="input.txt"):
    return open(fname, "r").readlines()


def read_into_lists(lines):
    return [np.array([int(x.strip()) for x in line.split()]) for line in lines]


def is_safe(report):
    deltas = np.diff(report)
    is_strict_monotonic = all(deltas > 0) or all(deltas < 0)

    return is_strict_monotonic and min(np.abs(deltas)) >= 1 and max(
        np.abs(deltas)) <= 3


def is_safe_with_removal(report):
    if is_safe(report):
        return True

    all_modified_reports = (np.delete(report, n) for n in range(len(report)))

    return any(is_safe(r) for r in all_modified_reports)


def part1(lines):
    reports = read_into_lists(lines)
    return sum(is_safe(r) for r in reports)


def part2(lines):
    reports = read_into_lists(lines)
    return sum(is_safe_with_removal(r) for r in reports)


print(part1(test_str))
print(part1(parselines()))
print(part2(test_str))
print(part2(parselines()))

# %%
