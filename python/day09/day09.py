#%%
import numpy as np
from itertools import zip_longest, groupby

test_str = "2333133121414131402"

def get_input():
    return open("input.txt").read()


def parse(s):
    s = s.strip()
    li = np.array([int(x) for x in s])
    return li[::2], li[1::2]


def find_first(array, n):
    vals, = np.where(np.array(array) == n)
    return min(vals)


def find_last(array):
    array: np.array = np.array(array)
    vals, = np.where(array >= 0)
    return max(vals)


def generate_blocks(n_files, n_unuseds):
    blocks = np.zeros(sum(n_files) + sum(n_unuseds), np.int64) - 1
    ii = 0
    iterator = enumerate(zip_longest(n_files, n_unuseds, fillvalue=0))
    for file_id, (n_file, n_free) in iterator:
        blocks[ii:ii + n_file] = file_id
        ii += n_file + n_free
    return blocks


def start_valid_island(a, window_size):
    for group in groupby(enumerate(a), lambda x: a[x[0]]):
        fid = group[0]
        if fid == -1:
            g = list(group[1])
            if len(g) >= window_size:
                return min(g)[0]


def checksum(blocks):
    blocks[np.where(blocks == -1)] = 0
    return sum(position * file_id for position, file_id in enumerate(blocks))


def part1(s):
    n_files, n_unuseds = parse(s)
    blocks = generate_blocks(n_files, n_unuseds)

    ii = 0
    while True:
        first_empty = find_first(blocks, -1)
        last_nonempty = find_last(blocks)
        if ii % 1000 == 0:
            print(f"{first_empty=}, {last_nonempty=}")
        if first_empty >= last_nonempty:
            break
        val_last_nomempty = blocks[last_nonempty]
        blocks[first_empty] = val_last_nomempty
        blocks[last_nonempty] = -1
        ii += 1

    return checksum(blocks)


def part2(s):
    n_files, n_unuseds = parse(s)
    blocks = generate_blocks(n_files, n_unuseds)

    ii = 0
    all_ids = np.unique(blocks)[1:]
    for fid in all_ids[::-1]:
        if ii % 1000 == 0:
            print(f"handling fid {fid}")
        run = np.where(blocks == fid)
        if len(run[0]) == 0:
            continue
        start = min(run[0])
        stop = max(run[0])
        first_open_spot = start_valid_island(blocks, len(run[0]))
        if first_open_spot is not None:
            if first_open_spot >= start:
                continue
            blocks[first_open_spot:first_open_spot + len(run[0])] = fid
            blocks[run] = 0
        ii += 1

    blocks[np.where(blocks == -1)] = 0
    f = open("test.txt", "w")
    f.write("\n".join(str(b) for b in blocks))
    return sum(position * file_id for position, file_id in enumerate(blocks))


print(part1(get_input()))
print(part2(get_input()))