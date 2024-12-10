#%%
import itertools as it
from functools import reduce

test_str = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""


def parse(s):
    return [parse_line(line) for line in s.splitlines()]


def parse_line(line):
    result, operands = line.split(":")
    result = int(result)
    operands = [int(x) for x in operands.split()]
    return result, operands


def mul(x, y):
    return x * y


def add(x, y):
    return x + y


def concat(x, y):
    return int(str(x) + str(y))


def generate_operator_variations(n, ops=(mul, add)):
    return it.product(ops, repeat=n)


def apply(left, op):
    right, func = op
    return func(left, right)


def calc(operators, operands):
    return reduce(apply, zip(operands[1:], operators), operands[0])


def check_candidate(result, operands, ops=(mul,add)):
    operator_variations = generate_operator_variations(len(operands) - 1, ops=ops)
    possible_results = (calc(var, operands) for var in operator_variations)
    if any(result == candidate for candidate in possible_results):
        return result
    else:
        return 0


def part1(s):
    candidates = parse(s)
    return sum(check_candidate(*c) for c in candidates)

def part2(s):
    candidates = parse(s)
    return sum(check_candidate(result, operands, (mul, add, concat)) for result, operands in candidates)

print(part1(test_str))
print(part1(open("input.txt").read()))
print(part2(test_str))
print(part2(open("input.txt").read()))

# %%
