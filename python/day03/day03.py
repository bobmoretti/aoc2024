# %%
import numpy as np
import re

test_str = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"


def get_input():
    return open("input.txt").read()


def parse_match(instr):
    first, last = instr.split(',')
    _, first = first.split('(')
    last, _ = last.split(')')
    return int(first), int(last)


REGEX = r"mul\([0-9]{1,3},[0-9]{1,3}\)"


def get_pairs(s):
    all_matches = re.findall(REGEX, s)

    pairs = np.array([parse_match(match) for match in all_matches])
    return pairs


def part1(s):
    pairs = get_pairs(s)
    return sum(np.prod(pairs, axis=1))


def part2(s):
    s = "do()" + s
    mul_ranges = [(m.start(0), m.end(0)) for m in re.finditer(REGEX, s)]
    do_indices = np.array([m.start(0) for m in re.finditer(r"do\(\)", s)])
    dont_indices = np.array([m.start(0) for m in re.finditer(r"don't\(\)", s)])
    pairs = get_pairs(s)

    def is_valid(index):
        start, _ = mul_ranges[index]

        do_search_val = np.searchsorted(do_indices, start, side='left')
        dont_search_val = np.searchsorted(dont_indices, start, side='left')
        if dont_search_val == 0:
            return True

        most_recent_do_index = int(do_indices[do_search_val - 1])
        most_recent_dont_index = int(dont_indices[dont_search_val - 1])
        valid = most_recent_do_index > most_recent_dont_index
        return valid

    return sum(a * b for ii, (a, b) in enumerate(pairs) if is_valid(ii))


def part2_split(s):
    do_substrs = s.split("do()")
    valids = [substr.split("don't()")[0] for substr in do_substrs]
    return sum(part1(substr) for substr in valids)


print(part1(test_str))
print(part1(get_input()))
print(part2(test_str))
print(part2(get_input()))
print(part2_split(test_str))
print(part2_split(get_input()))

# %%
