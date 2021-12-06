#!/usr/bin/env python3

import sys
import rich
from rich import print
import collections
import re
import enum
import dataclasses
from typing import Dict

class Marked(enum.Enum):
    CLEAR = enum.auto()
    MARKED = enum.auto()

@dataclasses.dataclass
class Tile:
    number: int
    row: int
    col: int
    status: Marked

class Board:
    def __init__(self, tiles):
        self.by_number = {}
        self.by_place = {}
        self.last_guess = None
        for tile in tiles:
            self.by_number[tile.number] = tile
            self.by_place[tile.row, tile.col] = tile

    def guess(self, number):
        self.last_guess = number
        if number in self.by_number:
            t = self.by_number[number]
            t.status = Marked.MARKED

    def won(self):
        for pattern in PATTERNS:
            if all(self.by_place[p].status == Marked.MARKED for p in pattern):
                return True
        return False

    def score(self):
        if not self.won():
            raise RuntimeError("Boards only have a score once they've won")
        subscore = sum(t.number for t in self.by_number.values() if t.status == Marked.CLEAR)
        return self.last_guess * subscore

    def __rich__(self):
        table = rich.table.Table.grid(padding = (0, 1))
        for _ in range(5):
            table.add_column()
        for r in range(5):
            row = []
            for c in range(5):
                t = self.by_place[r, c]
                if t.status == Marked.MARKED:
                    style = "bold"
                else:
                    style = "dim"
                row.append(f"[{style}]{t.number}[/{style}]")
            table.add_row(*row)
        table.add_row()
        return table
        

PATTERNS =(
    [[(r, c) for r in range(5)] for c in range(5)] +
    [[(r, c) for c in range(5)] for r in range(5)]
)

def parse_board(lines):
    assert len(lines) == 5
    tiles = []
    for row_idx, row in enumerate(lines):
        for col_idx, col in enumerate(re.split(" +", row)):
            if col == '':
                continue
            tiles.append(Tile(int(col), row_idx, col_idx, Marked.CLEAR))
    assert len(tiles) == 25
    return Board(tiles)


def parse_problem(lines):
    guesses = [int(n) for n in lines[0].split(",")]

    boards = []
    block = []
    for line in lines[2:]:
        if line == "\n":
            boards.append(parse_board(block))
            block = []
        else:
            block.append(line.strip())

    return guesses, boards



with open("data/day4_example.txt", 'r') as f:
    example_guesses, example_boards = parse_problem(f.readlines())

assert len(example_guesses) == 27
assert len(example_boards) == 3
assert all(not board.won() for board in example_boards)

for guess in example_guesses[0:5]:
    for board in example_boards:
        board.guess(guess)

assert all(not board.won() for board in example_boards)

for guess in example_guesses[5:11]:
    for board in example_boards:
        board.guess(guess)

assert all(not board.won() for board in example_boards)

for board in example_boards:
    board.guess(example_guesses[11])

assert any(board.won() for board in example_boards)
assert example_boards[2].score() == 4512

with open("data/day4.txt", 'r') as f:
    guesses, boards = parse_problem(f.readlines())
for guess in guesses:
    for board in boards:
        board.guess(guess)
    winners = [b for b in boards if b.won()]
    if winners:
        print(winners[0].score())
        break

remaining = example_boards
winners = []
for guess in example_guesses:
    for board in remaining:
        board.guess(guess)
        if board.won():
            winners.append(board)
    for board in winners:
        if board in remaining:
            remaining.remove(board)
    if len(remaining) == 1:
        last_winner = remaining[0]
        break
for guess in example_guesses:
    last_winner.guess(guess)
    if last_winner.won():
        break
assert last_winner.score() == 1924


remaining = boards
winners = []
for guess in guesses:
    for board in remaining:
        board.guess(guess)
        if board.won():
            winners.append(board)
    for board in winners:
        if board in remaining:
            remaining.remove(board)
    if len(remaining) == 1:
        last_winner = remaining[0]
        break
for guess in guesses:
    last_winner.guess(guess)
    if last_winner.won():
        break
print(last_winner.score())
