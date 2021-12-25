import copy
from rich import print
import itertools as it
import more_itertools as mit
import collections
import dataclasses
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
    "A": {(1, 3), (2, 3)},
    "B": {(1, 5), (2, 5)},
    "C": {(1, 7), (2, 7)},
    "D": {(1, 9), (2, 9)},
}

ROOMS_NEAR_PTS = {
    "A": {(1, 3)},
    "B": {(1, 5)},
    "C": {(1, 7)},
    "D": {(1, 9)},
}

ROOMS_FAR_PTS = {
    "A": {(2, 3)},
    "B": {(2, 5)},
    "C": {(2, 7)},
    "D": {(2, 9)},
}

POSITIONS_PTS = (
    HALLWAY_PTS | ROOMS_PTS["A"] | ROOMS_PTS["B"] | ROOMS_PTS["C"] | ROOMS_PTS["D"]
)


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
ROOMS_NEAR = {ct: {MAPPING[p] for p in pts} for ct, pts in ROOMS_NEAR_PTS.items()}
ROOMS_FAR = {ct: {MAPPING[p] for p in pts} for ct, pts in ROOMS_FAR_PTS.items()}
POSITIONS = {MAPPING[p] for p in POSITIONS_PTS}
NEIGHBORS = {MAPPING[k]: {MAPPING[p] for p in pts} for k, pts in NEIGHBORS_PTS.items()}
VALID_STOPS = POSITIONS - INVALID_STOPS
VALID_HALLWAY = HALLWAY - INVALID_STOPS


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

Point = Tuple[int, int]
Edge = Tuple[Point, Point]
CritterId = Tuple[str, int]
Critter = Tuple[Point, CritterId]

COSTS = {
    "A": 1,
    "B": 10,
    "C": 100,
    "D": 1000,
}


def cost_to_move(cid, start, end, state):
    if cid == state.last_moved:
        return None
    spaces = PATHS[start, end]
    if state.spaces_occupied(spaces):
        return None
    dist = len(spaces)
    return COSTS[cid[0]] * dist


def r_cid(cid):
    if cid:
        return f"{cid[0]}{cid[1]}"
    else:
        return "__"


@dataclasses.dataclass(frozen=True)
class State:
    occupied: Tuple[Optional[Critter]]
    last_moved: CritterId
    cost: int

    def spaces_occupied(self, spaces):
        return any(self.occupied[s] for s in spaces)

    def occupants(self, spaces):
        return [self.occupied[s][0] for s in spaces if self.occupied[s]]

    def room_done(self, ct):
        return all(self.occupied[s] == ct for s in ROOMS[ct])

    def all_done(self):
        return all(
            pos in ROOMS[cid[0]]
            for pos, cid in enumerate(self.occupied)
            if cid is not None
        )

    def estimate(self):
        return sum(
            COSTS[cid[0]] * min(len(PATHS[start, end]) for end in ROOMS[cid[0]])
            for start, cid in enumerate(self.occupied)
            if cid is not None
        )

    def __rich__(self):
        occ = ", ".join(r_cid(cid) for cid in self.occupied)
        return f"State(occupied={occ}, last={r_cid(self.last_moved)}, tot={self.cost + self.estimate()}, cost={self.cost})"


def successors(s):
    for start, cid in enumerate(s.occupied):
        if cid is None:
            continue
        if cid == s.last_moved:
            continue
        if start in ROOMS_FAR[cid[0]]:
            continue
        if s.room_done(cid[0]):
            continue
        if start in HALLWAY:
            occupants = s.occupants(ROOMS[cid[0]])
            if not occupants:
                positions = ROOMS_FAR[cid[0]]
            elif occupants[0] == cid[0]:
                positions = ROOMS_NEAR[cid[0]]
            else:
                continue
        else:
            positions = VALID_HALLWAY | ROOMS[cid[0]]
        for end in positions:
            if end == start:
                continue
            cost = cost_to_move(cid, start, end, s)
            if cost is None:
                continue
            new_pos = list(s.occupied)
            new_pos[start] = None
            new_pos[end] = cid
            yield State(tuple(new_pos), cid, s.cost + cost)


@dataclasses.dataclass(order=True)
class PrioritizedItem:
    priority: int
    item: Any = dataclasses.field(compare=False)


def search(init_state):
    visited = {}
    frontier = [PrioritizedItem(init_state.cost + init_state.estimate(), init_state)]
    its = 0
    while frontier:
        cur_state = heapq.heappop(frontier).item
        if cur_state.all_done():
            print(cur_state)
            return cur_state.cost
        known_cost = visited.get(cur_state.occupied, None)
        if known_cost is None or cur_state.cost < known_cost:
            visited[cur_state.occupied] = cur_state.cost
            for succ in successors(cur_state):
                heapq.heappush(
                    frontier, PrioritizedItem(succ.cost + succ.estimate(), succ)
                )

        its += 1
        if its % 1000 == 0:
            print(its, cur_state)


def main():
    init_map = [None] * len(POSITIONS)
    # init_map[MAPPING[1, 3]] = ('B', 1)
    # init_map[MAPPING[2, 3]] = ('A', 1)
    # init_map[MAPPING[1, 5]] = ('C', 1)
    # init_map[MAPPING[2, 5]] = ('D', 1)
    # init_map[MAPPING[1, 7]] = ('B', 2)
    # init_map[MAPPING[2, 7]] = ('C', 2)
    # init_map[MAPPING[1, 9]] = ('D', 2)
    # init_map[MAPPING[2, 9]] = ('A', 2)

    init_map[MAPPING[1, 3]] = ("C", 1)
    init_map[MAPPING[2, 3]] = ("D", 1)
    init_map[MAPPING[1, 5]] = ("A", 1)
    init_map[MAPPING[2, 5]] = ("D", 2)
    init_map[MAPPING[1, 7]] = ("B", 1)
    init_map[MAPPING[2, 7]] = ("B", 2)
    init_map[MAPPING[1, 9]] = ("C", 2)
    init_map[MAPPING[2, 9]] = ("A", 2)

    init_state = State(tuple(init_map), None, 0)

    print(init_state)

    assert cost_to_move(("B", 2), MAPPING[1, 7], MAPPING[0, 4], init_state) == 40
    assert not (init_state.all_done())

    print("tests done, running...")

    cost = search(init_state)
    print(cost)
