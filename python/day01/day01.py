# %%
import numpy as np

test_str = """3   4
4   3
2   5
1   3
3   9
3   3""".splitlines()


def parselines(fname="input.txt"):
    return open(fname, "r").readlines()


def read_into_lists(lines):
    return np.array([[int(x.strip()) for x in line.split()]
                     for line in lines]).T


def part1(lines):
    lists = read_into_lists(lines)

    sorted_lists = np.sort(lists, axis=1)
    return sum(np.abs(sorted_lists[1] - sorted_lists[0]))


def part2(lines):
    lists = read_into_lists(lines)
    
    def count_occurrences(needle, haystack):
        return sum(np.array(haystack) == needle)
    return sum(n*count_occurrences(n, lists[1]) for n in lists[0])


print(part1(test_str))
print(part1(parselines()))
print(part2(test_str))
print(part2(parselines()))

# %%
