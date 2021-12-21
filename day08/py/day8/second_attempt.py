import os
from typing import Callable, NewType
import itertools as it


def can(s):
    return "".join(sorted(s))


_DIGITS_TO_SEGMENTS = {
    0: can("abcefg"),
    1: can("cf"),
    2: can("acdeg"),
    3: can("acdfg"),
    4: can("bcdf"),
    5: can("abdfg"),
    6: can("abdefg"),
    7: can("acf"),
    8: can("abcdefg"),
    9: can("abcdfg"),
}

_SEGMENTS_TO_DIGITS = {v: k for k, v in _DIGITS_TO_SEGMENTS.items()}

_UNIQUES = [
    _DIGITS_TO_SEGMENTS[1],
    _DIGITS_TO_SEGMENTS[4],
    _DIGITS_TO_SEGMENTS[7],
    _DIGITS_TO_SEGMENTS[8],
]


def parse(line):
    observed, output = line.strip().split(" | ")
    observed = list(can(s) for s in observed.split(" "))
    output = list(can(s) for s in output.split(" "))
    return observed, output


def part1(observed, output) -> int:
    return sum(len(js) in {2, 3, 4, 7} for js in output)


def unproject(segments_to_signals, output):
    return can(segments_to_signals[js] for js in output)


def part2_apply_mapping(observed, output, segments_to_signals):
    # Given that we can figure out which digits some of the
    # observed signals must produce by looking at their lengths,
    # reject if the mapping fails to produce those digits
    observed_proj = (unproject(segments_to_signals, s) for s in observed)
    for proj in observed_proj:
        for unique in _UNIQUES:
            if len(proj) == len(unique):
                if proj != unique:
                    return None

    # Reject if the mapping doesn't create comprehensible digits
    output_proj = (unproject(segments_to_signals, js) for js in output)
    digits = [_SEGMENTS_TO_DIGITS.get(rp, None) for rp in output_proj]
    if any(d is None for d in digits):
        return None

    # Return what we found
    return int("".join(str(d) for d in digits))


def part2(observed, jumbled_segments) -> int:
    for perm in it.permutations("abcdefg"):
        result = part2_apply_mapping(
            observed, jumbled_segments, dict(zip("abcdefg", perm))
        )
        if result is not None:
            return result
    raise RuntimeError("No solution found")


def read_file(filename: str):
    with open(filename, "r") as f:
        return [parse(s.strip()) for s in f.readlines()]


def solve(data, f) -> int:
    return sum(f(sig, jum) for sig, jum in data)


def main():
    ex1 = read_file("../example1.txt")
    ex2 = read_file("../example2.txt")
    inp = read_file("../input.txt")

    assert solve(ex1, part1) == 0
    assert solve(ex2, part1) == 26
    assert solve(inp, part1) == 412

    assert solve(ex1, part2) == 5353
    assert solve(ex2, part2) == 61229
    assert solve(inp, part2) == 978171
