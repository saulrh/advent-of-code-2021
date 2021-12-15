#!/usr/bin/env python3

from box import Box
import sqlite3
from rich import print


db = sqlite3.connect("data.db")
cur = db.cursor()

risks_raw = cur.execute(" ".join([
    "SELECT",
    "  risks.r,",
    "  risks.c,",
    "  risks.v",
    "FROM risks",
]))
risks = {(c[0], c[1]): Box({'r': c[0], 'c': c[1], 'v': c[2]}) for c in risks_raw}

costs_raw = cur.execute(" ".join([
    "SELECT",
    "  costs.r,",
    "  costs.c,",
    "  costs.v,",
    "  neighbors.disp",
    "FROM costs",
    "INNER JOIN neighbors on costs.neighbor = neighbors.rowid"
]))
costs = {(c[0], c[1]): Box({'r': c[0], 'c': c[1], 'v': c[2], 'n': c[3]}) for c in costs_raw}

route_raw = cur.execute("select r, c from route")
route = {(p[0], p[1]) for p in route_raw}

maxr = cur.execute("select max(r) from costs")
maxc = cur.execute("select max(c) from costs")

for row in range(1, maxr + 1):
    line = []
    for col in range(1, maxc + 1)
        c = risks.get((row, col), Box(r=row, c=col, v=0))
        color = "bold green" if (row, col) in route else "dim white"
        line.append(f"[{color}]{c.v:3} [/{color}]")
    print(" ".join(line))

print()

for row in range(1, maxr + 1):
    line = []
    for col in range(1, maxc + 1)
        c = costs.get((row, col), Box(r=row, c=col, v=0, n=" "))
        color = "bold green" if (row, col) in route else "dim white"
        line.append(f"[{color}]{c.v:3}{c.n}[/{color}]")
    print(" ".join(line))

