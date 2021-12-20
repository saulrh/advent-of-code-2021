import pathlib
import collections
import dataclasses
import rich
from rich import print
from typing import Tuple, List, Dict, NewType, Generator

Point = NewType("Point", Tuple[int, int])

_NEIGHBORHOOD = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 0),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
]


def neighborhood(pt) -> Generator[Point, None, None]:
    for n in _NEIGHBORHOOD:
        yield tuple(sum(vs) for vs in zip(pt, n))


@dataclasses.dataclass
class Problem:
    grid: Dict[Point, bool]
    decompress: List[bool]
    infinity: bool

    @property
    def min_row(self) -> int:
        return min(p[0] for p in self.grid.keys())

    @property
    def min_col(self) -> int:
        return min(p[1] for p in self.grid.keys())

    @property
    def max_row(self) -> int:
        return max(p[0] for p in self.grid.keys())

    @property
    def max_col(self) -> int:
        return max(p[1] for p in self.grid.keys())

    def get(self, arg1, arg2=None) -> bool:
        if arg2 is not None:
            return self.grid.get((arg1, arg2), self.infinity)
        else:
            return self.grid.get(arg1, self.infinity)

    @property
    def expanded_keys(self):
        pts = set()
        for pt in self.grid.keys():
            pts.update(neighborhood(pt))
        return pts

    def step(self):
        next_grid = {}
        for pt in self.expanded_keys:
            idx = bits_to_number(self.get(n) for n in neighborhood(pt))
            next_grid[pt] = self.decompress[idx]
        self.infinity = self.decompress[
            bits_to_number(self.infinity for n in neighborhood((0, 0)))
        ]
        self.grid = next_grid

    @property
    def lit_bits(self):
        return sum(self.grid.values())

    def __rich__(self) -> str:
        output = []
        for row in range(self.min_row, self.max_row + 1):
            output_row = []
            for col in range(self.min_col, self.max_col + 1):
                output_row.append(self.get(row, col))
            output.append(bits_to_hashes(output_row))
        return "\n".join("".join(r) for r in output)


_BIT_VALUES = list(reversed([2 ** idx for idx in range(9)]))


def bits_to_number(bits):
    return sum(v * b for v, b in zip(_BIT_VALUES, bits))


def hashes_to_bits(s):
    return [c == "#" for c in s]


def bits_to_hashes(bits):
    return ["#" if b else "·" for b in bits]


def load_problem(text):
    it = (s.strip() for s in text.splitlines())
    decompress = hashes_to_bits(next(it))
    next(it)
    grid = {}
    for row, line in enumerate(it):
        for col, char in enumerate(line):
            grid[row, col] = char == "#"
    return Problem(grid, decompress, False)


def main():
    ex = load_problem(pathlib.Path("../example.txt").read_text())
    print(ex)
    print(ex.lit_bits)
    print()
    ex.step()
    print(ex)
    print(ex.lit_bits)
    print()
    ex.step()
    print(ex)
    print(ex.lit_bits)
    print()

    inp = load_problem(pathlib.Path("../input.txt").read_text())
    print(inp)
    print()
    inp.step()
    print(inp)
    print()
    inp.step()
    print(inp.lit_bits)
