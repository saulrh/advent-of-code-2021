import z3
import pathlib
import itertools as it
import dataclasses
import functools
import rich
import rich.progress
from rich import print

NodeSort = z3.DeclareSort('Node')


@dataclasses.dataclass
class Beacon:
    scanner_idx: int
    idx: int
    rel_pos: tuple[int, int, int]
    v: tuple[z3.Const, z3.Const, z3.Const] = dataclasses.field(init=False)

    v_reified: tuple[int, int, int] = (0,0,0)

    def __post_init__(self):
        self.v = make_triple(f"beacon,{self.scanner_idx},{self.idx}")

    def reify(self, m):
        self.v_reified = reify_triple(self.v, m)


@dataclasses.dataclass(unsafe_hash=True)
class Scanner:
    idx: int = dataclasses.field(compare=True)
    beacons: list[Beacon] = dataclasses.field(compare=False)
    v: tuple[z3.Const, z3.Const, z3.Const] = dataclasses.field(init=False, compare=False)
    forward: tuple[z3.Const, z3.Const, z3.Const] = dataclasses.field(init=False, compare=False)
    up: tuple[z3.Const, z3.Const, z3.Const] = dataclasses.field(init=False, compare=False)
    node: z3.Const = dataclasses.field(init=False, compare=False)

    v_reified: tuple[int, int, int] = (0,0,0)
    f_reified: tuple[int, int, int] = (0,0,0)
    u_reified: tuple[int, int, int] = (0,0,0)

    def __post_init__(self):
        self.v = make_triple(f"scanner,v,{self.idx}")
        self.forward = make_triple(f"scanner,f,{self.idx}")
        self.up = make_triple(f"scanner,u,{self.idx}")
        self.node = z3.Const(f"scanner,node,{self.idx}", NodeSort)

    def reify(self, m):
        self.v_reified = reify_triple(self.v, m)
        self.f_reified = reify_triple(self.forward, m)
        self.u_reified = reify_triple(self.up, m)

    @functools.cached_property
    def right(self):
        return cross(self.forward, self.up)

    def transform(self, p):
        return (
            self.forward[0] * p[0] + self.up[0] * p[1] + self.right[0] * p[2],
            self.forward[1] * p[0] + self.up[1] * p[1] + self.right[1] * p[2],
            self.forward[2] * p[0] + self.up[2] * p[1] + self.right[2] * p[2],
        )

    def __repr__(self):
        return f"scanner{self.idx}"


def make_triple(name):
    return (z3.Const(f"{name},x", z3.IntSort()),
            z3.Const(f"{name},y", z3.IntSort()),
            z3.Const(f"{name},z", z3.IntSort()))


def reify_triple(t, m):
    return tuple(int(m.eval(value, model_completion=True).as_long()) for value in t)


def ingest(filestring):
    scanners = []
    curscan = []
    for line in filestring.splitlines():
        if line.startswith('---'):
            curscan = []
            scanners.append(curscan)
        elif not line:
            pass
        else:
            curscan.append(tuple(int(w) for w in line.split(',')))
    return scanners

def build(scanners):
    for s_idx, beacons in enumerate(scanners):
        bs = []
        for b_idx, beacon in enumerate(beacons):
            bs.append(Beacon(s_idx, b_idx, beacon))
        yield Scanner(s_idx, bs)

def eq(a, b):
    return z3.And(*(aa == bb for aa, bb in zip(a, b)))

def plus(a, b):
    return tuple(aa + bb for aa, bb in zip(a, b))

def minus(a, b):
    return tuple(aa - bb for aa, bb in zip(a, b))

def mag(a):
    return sum(z3.Abs(aa) for aa in a)

def negate(a):
    return tuple(-aa for aa in a)

def cross(a, b):
    return (
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    )

def dot(a, b):
    return tuple(aa * bb for aa, bb in zip(a, b))


def in_range(scanner, beacon):
    return z3.And(*(z3.And(-1000 <= ss - bb, ss - bb <= 1000) for ss, bb in zip(scanner.v, beacon.v)))


def main():
    scanners = list(build(ingest(pathlib.Path('../example_1.txt').read_text())))

    print(len(scanners))
    print(sum(len(s.beacons) for s in scanners))

    s = z3.Solver()

    # we'll use the suggested convention and use scanner 0 as the
    # origin
    s.assert_and_track(eq(scanners[0].v, (0, 0, 0)), 'origin v')
    s.assert_and_track(eq(scanners[0].forward, (1, 0, 0)), 'origin f')
    s.assert_and_track(eq(scanners[0].up, (0, 0, 1)), 'origin u')

    # express constraints on scanner facing
    for scanner in rich.progress.track(scanners, description="up and facing"):
        # facing and up vectors are unit vectors, which in integers
        # means they're one of the three basis vectors times either
        # one or minus one.
        s.assert_and_track(mag(scanner.forward) == 1, f"scanner {scanner.idx} forward unit")
        s.assert_and_track(mag(scanner.up) == 1, f"scanner {scanner.idx} up unit")
        # additionally, up is perpendicular to f.
        s.assert_and_track(z3.Not(eq(scanner.up, scanner.forward)), f"scanner {scanner.idx} up != f")
        s.assert_and_track(z3.Not(eq(scanner.up, negate(scanner.forward))), f"scanner {scanner.idx} up != -f")
        # There are six basis vectors, so there are six possibilities
        # for f and four (six minus two) possibilities for up, giving
        # us 24 total orientations, which matches R^3 so this should
        # be satisfactory.

    # Combine each beacon's observed relative position with its
    # scanner's absolute position, facing, and up-vector to constrain
    # the beacon's absolute position.
    for scanner in rich.progress.track(scanners, description="beacon abs positions"):
        for beacon in scanner.beacons:
            s.assert_and_track(
                eq(beacon.v, plus(scanner.v, scanner.transform(beacon.rel_pos))),
                f"scanner{scanner.idx} beacon{beacon.idx} position",
            )

    # relate nearby scanners to each other
    n_relations = len(scanners) * (len(scanners) - 1) / 2
    for sa, sb in rich.progress.track(it.product(scanners, repeat=2), description="scanner overlaps", total=n_relations):
        if sa == sb:
            continue
        # if scanner A is placed such that one of its beacons should
        # be visible from scanner B, ensure that scanner B has a
        # beacon placed there.
        for ba in sa.beacons:
            s.add(z3.Implies(in_range(ba, sb), z3.Or(*(ba.v == bb.v for bb in sb.beacons))))

    # all of the scanners are connected to each other. our constraints
    # are all of the form "if two scanners are close together, their
    # beacons need to overlap", which the scanners would happily
    # satisfy by flying off into space if not for this constraint. So
    # we need this to keep things contained.
    #
    # mechanically, this says that two scanners are connected if and
    # only if they can see enough of each other's probes. we then
    # require all scanners to be able to reach scanner 0 by jumping
    # between connected scanners.
    conns = {}
    connected = z3.Function('connected', NodeSort, NodeSort, z3.BoolSort())
    connected_closure = z3.TransitiveClosure(connected)
    for sa, sb in rich.progress.track(it.product(scanners, repeat=2), description="scanners all connected", total=n_relations):
        if sa == sb:
            continue
        conns[sa, sb] = z3.Sum(*(z3.If(in_range(ba, sb), 1, 0) for ba in sa.beacons))
        s.add(z3.If(conns[sa, sb] >= 12,
                    connected(sa.node, sb.node),
                    z3.Not(connected(sa.node, sb.node))))

    for scanner in scanners:
        s.add(connected_closure(scanners[0].node, scanner.node))

    with rich.progress.Progress() as p:
        p.add_task("solving...", total=None)
        satisfiable = s.check()

    if satisfiable == z3.unsat:
        print("[red]unsat[/]")
        core = s.unsat_core()
        print(core)
        print(len(core))

    else:
        print("[green]sat[/]")
        model = s.model()
        for s in scanners:
            s.reify(model)
            for b in s.beacons:
                b.reify(model)

        unique_positions = set()
        for sc in scanners:
            print(sc.idx, sc.v_reified)
            for bc in sc.beacons:
                unique_positions.add(bc.v_reified)

        print("number of unique beacons:")
        print(len(unique_positions))

        print(conns.keys())

        for sa, sb in it.product(scanners, repeat=2):
            if sa == sb:
                continue
            conn = conns[sa, sb]
            print(model.eval(conn))
            print(model.eval(connected(sa, sb)))
            # print(conn)

        assert scanners[0].v_reified == (68, -1246, -43)

        


if __name__ == "__main__":
    main()
