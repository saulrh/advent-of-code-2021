import rich
from rich import print
import itertools as it
from typing import Set, List, Iterable, Tuple, FrozenSet


def read_letters(w: str) -> FrozenSet[int]:
    return frozenset(ord(c) - ord("a") for c in w)


def read_line(line) -> Tuple[FrozenSet[FrozenSet[int]], List[FrozenSet[str]]]:
    first, second = line.split(" | ")
    signal_sets = frozenset(read_letters(w) for w in first.strip().split(" "))
    output_sets = [frozenset(w.upper()) for w in second.strip().split(" ")]
    return signal_sets, output_sets


def read(filename):
    with open(filename, "r") as f:
        return [read_line(l) for l in f.readlines()]


def part1(output_sets_list):
    count = 0
    for output_sets in output_sets_list:
        for outputs in output_sets:
            if len(outputs) in [2, 4, 3, 7]:
                count += 1
    return count


_WIRES = list(range(7))

_SEGMENTS = list("ABCDEFG")

_DIGITS = [
    frozenset("ABCEFG"),
    frozenset("CF"),
    frozenset("ACDEG"),
    frozenset("ACDFG"),
    frozenset("BCDF"),
    frozenset("ABDFG"),
    frozenset("ABDEFG"),
    frozenset("ACF"),
    frozenset("ABCDEFG"),
    frozenset("ABCDFG"),
]


def consistent_with_signals(signals: Set[int], digit: Set[str], p: List[str]) -> bool:
    for signal in signals:
        if p[signal] not in digit:
            return False
    return True


def consistent_with_segment_sets(
    jumbled_segment_sets: Iterable[Iterable[str]], p: List[str]
) -> bool:
    return all(
        rerender_jumbled_segments(p, js) in _DIGITS for js in jumbled_segment_sets
    )


def solve(signal_sets: Iterable[Set[int]], segment_sets: Iterable[Set[str]]):
    poss: List[List[str]] = list(list(p) for p in it.permutations(_SEGMENTS))

    # We can reject any possibility that isn't consistent with the
    # digits that we can find becuase their signal-sets have a unqiue
    # size
    for signals in signal_sets:
        if len(signals) == 2:
            poss = [p for p in poss if consistent_with_signals(signals, _DIGITS[1], p)]
        if len(signals) == 4:
            poss = [p for p in poss if consistent_with_signals(signals, _DIGITS[4], p)]
        if len(signals) == 3:
            poss = [p for p in poss if consistent_with_signals(signals, _DIGITS[7], p)]
        if len(signals) == 7:
            poss = [p for p in poss if consistent_with_signals(signals, _DIGITS[8], p)]

    # we can reject any possibility that wouldn't produce a coherent output
    poss = [p for p in poss if consistent_with_segment_sets(segment_sets, p)]

    # some lines are underspecified, but there should still only be a
    # single unique output, because the problem is nice like that
    outputs = {intended_number(p, segment_sets) for p in poss}
    assert len(outputs) == 1

    return poss[0]


_DISPLAY = "\n".join(
    [
        " AAAA ",
        "B    C",
        "B    C",
        " DDDD ",
        "E    F",
        "E    F",
        " GGGG ",
    ]
)


def format_mapping(mapping: List[str]) -> str:
    result = _DISPLAY
    for idx, c in enumerate(mapping):
        result = result.replace(c, str(idx))
    return result


def format_segments(segments: Iterable[str]) -> str:
    result = _DISPLAY
    for seg in "ABCDEFG":
        if seg not in segments:
            result = result.replace(seg, " ")
    return result


def render_signals(mapping: List[str], signals: Iterable[int]) -> Set[str]:
    return {mapping[s] for s in signals}


def unrender_segments(mapping: List[str], segments: Iterable[str]) -> Set[int]:
    revmap = {seg: idx for idx, seg in enumerate(mapping)}
    return {revmap[seg] for seg in segments}


def rerender_jumbled_segments(
    mapping: List[str], jumbled_segments: Iterable[str]
) -> Set[str]:
    intended_signals = unrender_segments(list("ABCDEFG"), jumbled_segments)
    return render_signals(mapping, intended_signals)


def intended_digit(mapping: List[str], jumbled_segments: Iterable[str]) -> int:
    actual_segments = rerender_jumbled_segments(mapping, jumbled_segments)
    return _DIGITS.index(actual_segments)


def intended_number(
    mapping: List[str], jumbled_segment_sets: Iterable[Iterable[str]]
) -> int:
    result = 0
    for jumbled_segments in jumbled_segment_sets:
        result *= 10
        i_d = intended_digit(mapping, jumbled_segments)
        result += i_d
    return result


def hcat(*strs: str) -> str:
    splitted = [s.splitlines() for s in strs]
    result = ""
    for lines in zip(*splitted):
        result += " ".join(lines)
        result += "\n"
    return result


def main():
    ex1 = read("../example1.txt")
    ex2 = read("../example2.txt")
    data = read("../input.txt")
    assert part1([e[1] for e in ex1]) == 0
    assert part1([e[1] for e in ex2]) == 26
    assert part1([e[1] for e in data]) == 412
    ex1_soln = solve(ex1[0][0], ex1[0][1])

    digits = [
        format_segments(rerender_jumbled_segments(ex1_soln, js)) for js in ex1[0][1]
    ]
    print(hcat(format_mapping(ex1_soln), *digits))
    assert intended_number(ex1_soln, ex1[0][1]) == 5353

    assert (
        sum(intended_number(solve(prob[0], prob[1]), prob[1]) for prob in ex2) == 61229
    )

    print(sum(intended_number(solve(prob[0], prob[1]), prob[1]) for prob in data))
