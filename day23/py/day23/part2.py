import operator
import copy
from rich import print
import itertools as it
import functools as ft
import more_itertools as mit
import collections
import attr
from typing import List, Tuple, Dict, Any, Optional
import enum
import heapq

HALLWAY_PTS = {(0, col) for col in range(1, 11 + 1)}

INVALID_STOPS_PTS = {
    (0, 3),
    (0, 5),
    (0, 7),
    (0, 9),
}

ROOMS_PTS = {
    ord("A"): {(1, 3), (2, 3), (3, 3), (4, 3)},
    ord("B"): {(1, 5), (2, 5), (3, 5), (4, 5)},
    ord("C"): {(1, 7), (2, 7), (3, 7), (4, 7)},
    ord("D"): {(1, 9), (2, 9), (3, 9), (4, 9)},
}

ROOMS_DEEP_PTS = {
    ord("A"): (4, 3),
    ord("B"): (4, 5),
    ord("C"): (4, 7),
    ord("D"): (4, 9),
}

POSITIONS_PTS = HALLWAY_PTS | ft.reduce(operator.or_, ROOMS_PTS.values())


def adj(p1, p2):
    return sum(abs(v1 - v2) for v1, v2 in zip(p1, p2)) == 1


EDGES_PTS = [vs for vs in it.product(POSITIONS_PTS, POSITIONS_PTS) if adj(*vs)]
NEIGHBORS_PTS = {
    k: [v[1] for v in vs] for k, vs in it.groupby(EDGES_PTS, key=lambda p: p[0])
}

MAPPING = {p: idx for idx, p in enumerate(sorted(POSITIONS_PTS))}

HALLWAY = {MAPPING[p] for p in HALLWAY_PTS}
INVALID_STOPS = {MAPPING[p] for p in INVALID_STOPS_PTS}
ROOMS = {ct: {MAPPING[p] for p in pts} for ct, pts in ROOMS_PTS.items()}
POSITIONS = {MAPPING[p] for p in POSITIONS_PTS}
NEIGHBORS = {MAPPING[k]: {MAPPING[p] for p in pts} for k, pts in NEIGHBORS_PTS.items()}
VALID_HALLWAY = HALLWAY - INVALID_STOPS
ROOMS_DEEP = {k: MAPPING[v] for k, v in ROOMS_DEEP_PTS.items()}


def path_between(start, end):
    visited = set([start])
    frontier = collections.deque([(start, [])])
    while frontier:
        cur, path = frontier.popleft()
        if cur == end:
            return path
        for neighbor in NEIGHBORS[cur]:
            if neighbor not in visited:
                visited.add(neighbor)
                frontier.append((neighbor, [*path, neighbor]))


PATHS = {(p1, p2): path_between(p1, p2) for p1, p2 in it.product(POSITIONS, POSITIONS)}


@attr.s(cmp=True, auto_attribs=True, frozen=True, slots=True)
class CritterId:
    t: int
    n: int

    def __str__(self):
        return f"{chr(self.t)}{self.n}"


Point = int
Critter = Tuple[Point, CritterId]

COSTS = {
    ord("A"): 1,
    ord("B"): 10,
    ord("C"): 100,
    ord("D"): 1000,
}


def cost_to_move(cid, start, end, state):
    spaces = PATHS[start, end]
    if state.spaces_occupied(spaces):
        return None
    dist = len(spaces)
    return COSTS[cid.t] * dist


@attr.s(auto_attribs=True, slots=True, frozen=True)
class State:
    rooms: Tuple[Optional[CritterId]]
    cost: int

    def spaces_occupied(self, spaces):
        return any(self.rooms[s] is not None for s in spaces)

    def room_polluted(self, ct):
        return any(
            self.rooms[s].t != ct for s in ROOMS[ct] if self.rooms[s] is not None
        )

    def all_done(self):
        return all(
            pos in ROOMS[cid.t] for pos, cid in enumerate(self.rooms) if cid is not None
        )

    def estimate(self):
        return sum(
            COSTS[cid.t] * min(len(PATHS[start, end]) for end in ROOMS[cid.t])
            for start, cid in enumerate(self.rooms)
            if cid is not None
        )

    def __rich__(self):
        def r_cid(c):
            return str(c) if c else "__"

        def dg(s):
            return f"[#444444]{s}[/#444444]"

        hall = []
        for h in VALID_HALLWAY:
            hall.append(r_cid(self.rooms[h]))
        hall = [
            *hall[0:2],
            dg("a"),
            hall[2],
            dg("b"),
            hall[3],
            dg("c"),
            hall[4],
            dg("d"),
            *hall[5:7],
        ]
        hall = " ".join(hall)

        rooms = []
        for c, rs in sorted(ROOMS.items()):
            rooms.append(
                dg(chr(c).lower())
                + " "
                + " ".join(r_cid(self.rooms[r]) for r in sorted(rs))
            )
        rooms = " | ".join(rooms)

        return f"State({hall} | {rooms} | tot={self.cost + self.estimate()}, cost={self.cost}, est={self.estimate()})"


def successors(s):
    for start, cid in enumerate(s.rooms):
        if cid is None:
            continue

        in_goal_room = start in ROOMS[cid.t]
        in_hallway = start in HALLWAY
        goal_room_polluted = s.room_polluted(cid.t)

        open_in_room = [r for r in ROOMS[cid.t] if s.rooms[r] is None]
        deepest_open_in_room = max(
            open_in_room, key=lambda r: len(PATHS[0, r]), default=None
        )
        goal_room = {deepest_open_in_room} if deepest_open_in_room else set()

        if in_hallway:
            if goal_room_polluted:
                continue
            else:
                positions = goal_room
        elif in_goal_room:
            if goal_room_polluted:
                positions = VALID_HALLWAY
            else:
                continue
        else:
            # In a non-goal room
            if goal_room_polluted:
                positions = VALID_HALLWAY
            else:
                positions = VALID_HALLWAY | goal_room

        for end in positions:
            cost = cost_to_move(cid, start, end, s)
            if cost is None:
                continue
            new_pos = list(s.rooms)
            new_pos[start] = None
            new_pos[end] = cid
            yield State(tuple(new_pos), s.cost + cost)


@attr.s(auto_attribs=True, cmp=True, frozen=True, slots=True)
class PrioritizedItem:
    priority: int
    item: Any = attr.ib(order=False)


def search(init_state):
    visited = set()
    frontier = [PrioritizedItem(init_state.cost + init_state.estimate(), init_state)]
    its = 0
    while frontier:
        cur_state = heapq.heappop(frontier).item
        if cur_state.all_done():
            print(cur_state)
            return cur_state.cost
        if cur_state.rooms not in visited:
            visited.add(cur_state.rooms)
            for succ in successors(cur_state):
                heapq.heappush(
                    frontier, PrioritizedItem(succ.cost + succ.estimate(), succ)
                )

        its += 1
        if its % 10000 == 0:
            # print(f"  frontier: {len(frontier)}, visited: {len(visited)}, cost: {cur_state.cost}")
            print(its, len(frontier), cur_state)


def main():
    init_map = [None] * len(POSITIONS)

    # init_map[MAPPING[1, 3]] = CritterId(ord('B'), 1)
    # init_map[MAPPING[4, 3]] = CritterId(ord('A'), 1)
    # init_map[MAPPING[1, 5]] = CritterId(ord('C'), 1)
    # init_map[MAPPING[4, 5]] = CritterId(ord('D'), 3)
    # init_map[MAPPING[1, 7]] = CritterId(ord('B'), 3)
    # init_map[MAPPING[4, 7]] = CritterId(ord('C'), 3)
    # init_map[MAPPING[1, 9]] = CritterId(ord('D'), 4)
    # init_map[MAPPING[4, 9]] = CritterId(ord('A'), 4)
    # expect = lambda s: s == 44169

    init_map[MAPPING[1, 3]] = CritterId(ord("C"), 1)
    init_map[MAPPING[4, 3]] = CritterId(ord("D"), 1)
    init_map[MAPPING[1, 5]] = CritterId(ord("A"), 1)
    init_map[MAPPING[4, 5]] = CritterId(ord("D"), 2)
    init_map[MAPPING[1, 7]] = CritterId(ord("B"), 1)
    init_map[MAPPING[4, 7]] = CritterId(ord("B"), 2)
    init_map[MAPPING[1, 9]] = CritterId(ord("C"), 2)
    init_map[MAPPING[4, 9]] = CritterId(ord("A"), 2)
    expect = lambda s: s == 50190

    init_map[MAPPING[2, 3]] = CritterId(ord("D"), 1)
    init_map[MAPPING[3, 3]] = CritterId(ord("D"), 2)
    init_map[MAPPING[2, 5]] = CritterId(ord("C"), 2)
    init_map[MAPPING[3, 5]] = CritterId(ord("B"), 2)
    init_map[MAPPING[2, 7]] = CritterId(ord("B"), 4)
    init_map[MAPPING[3, 7]] = CritterId(ord("A"), 2)
    init_map[MAPPING[2, 9]] = CritterId(ord("A"), 3)
    init_map[MAPPING[3, 9]] = CritterId(ord("C"), 4)

    init_state = State(tuple(init_map), 0)

    print(init_state)

    assert (
        cost_to_move(CritterId(ord("B"), 2), MAPPING[1, 7], MAPPING[0, 4], init_state)
        == 40
    )
    assert not (init_state.all_done())

    print("tests done, running...")

    cost = search(init_state)
    print(cost)

    assert expect(cost)
