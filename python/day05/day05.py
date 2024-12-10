# %%

from collections import defaultdict

test_str = """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""


def get_input():
    return open("input.txt").read()


def parse(s):
    s_rules, s_pagelists = s.split("\n\n")

    def make_rule(line):
        x, y = line.split("|")
        return int(x), int(y)

    rules = [make_rule(line) for line in s_rules.splitlines()]
    pagelists = [[int(x) for x in line.split(',')]
                 for line in s_pagelists.splitlines()]
    return rules, pagelists


def assemble_dict(rules):
    rule_dict = defaultdict(list)
    for a, b in rules:
        rule_dict[a].append(b)
    return rule_dict


def get_middle(rule):
    return rule[len(rule) // 2]


def is_valid_order(pagelist, lookup):
    for ii in range(2, len(pagelist) + 1):
        sublist = pagelist[:ii]
        must_follow = lookup[sublist[-1]]
        # if anything in the first n items of the sublist is in the must_follow, we fail
        if any(page in must_follow for page in sublist[:-1]):
            return False
    return True


def part1(s):
    rules, pagelists = parse(s)
    lookup = assemble_dict(rules)

    return sum(
        get_middle(pagelist) for pagelist in pagelists
        if is_valid_order(pagelist, lookup))


def part2(s):
    rules, pagelists = parse(s)
    lookup = assemble_dict(rules)

    def fix_ordering(pagelist):
        for ii in range(2, len(pagelist) + 1):
            for jj in range(ii):
                candidate = pagelist[ii - 1]
                must_follow = lookup[candidate]
                if pagelist[jj] in must_follow:
                    pagelist.insert(jj, candidate)
                    del pagelist[ii]
        return pagelist

    return sum(
        get_middle(fix_ordering(pagelist)) for pagelist in pagelists
        if not is_valid_order(pagelist, lookup))


print(part1(test_str))
print(part1(get_input()))
print(part2(test_str))
print(part2(get_input()))

# %%
