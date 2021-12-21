import pathlib
import re
import collections
import dataclasses
import numpy as np
import itertools as it
import more_itertools as mit
import rich
from rich import print
import operator as op
import functools as ft

EYE = np.eye(3, dtype=np.int32)

def rotations():
    permutations = list(np.array(p) for p in it.permutations(EYE))
    row_invert = [
        np.array([[-1, 0, 0], [0, 1, 0], [0, 0, 1]], dtype=np.int32),
        np.array([[1, 0, 0], [0, -1, 0], [0, 0, 1]], dtype=np.int32),
        np.array([[1, 0, 0], [0, 1, 0], [0, 0, -1]], dtype=np.int32),
    ]
    inversions = [ft.reduce(op.mul, ms, EYE) for ms in mit.powerset(row_invert)]
    symmetries = [np.dot(r, p) for r, p in it.product(inversions, permutations)]
    return [m for m in symmetries if np.linalg.det(m) == 1]

ROTATIONS = rotations()

def assert_vec_eq(a, b, message = None):
    assert np.all(a == b), message or f"{r_pt(a)} != {r_pt(b)}"

def load_problem(s):
    output = []
    scanner = []
    for line in s.splitlines():
        line = line.strip()
        if not line:
            continue
        elif "scanner" in line:
            if scanner:
                output.append(np.stack(scanner).T)
            scanner = []
        else:
            scanner.append(np.array([int(w) for w in line.split(',')], dtype=np.int32))
    output.append(np.stack(scanner).T)
    return output


def coincidences(s1, s2):
    s1_set = {tuple(c) for c in s1.T}
    s2_set = {tuple(c) for c in s2.T}
    return(len(s1_set & s2_set))        

def overlap(s1, s2):
    s1_len = s1.shape[1]
    s2_len = s2.shape[1]
    for rot in ROTATIONS:
        s2_rotated = rot.dot(s2)
        for p1 in s1.T:
            for p2 in s2_rotated.T:
                translation = p1 - p2
                s2_in_s1 = s2_rotated + np.tile(translation, (s2_len, 1)).T
                if coincidences(s1, s2_in_s1) >= 12:
                    return rot, translation
    return None

def r_pt(pt):
    return "[" + ",".join(str(i) for i in pt) + "]"

def slam(sensors):
    # Define sensor 0 to be at 0, 0, 0
    frontier = collections.deque([0])
    positions = {0: (EYE, np.zeros(3, dtype=np.int32))}
    while frontier:
        cur_idx = frontier.pop()
        cur = sensors[cur_idx]
        for other_idx, other in enumerate(sensors):
            if other_idx not in positions:
                ov = overlap(cur, other)
                if ov:
                    other_rot, other_in_s0 = ov
                    other_len = other.shape[1]
                    other = other_rot.dot(other)
                    other = other + np.tile(other_in_s0, (other_len, 1)).T
                    sensors[other_idx] = other
                    positions[other_idx] = (other_rot, other_in_s0)
                    frontier.append(other_idx)
                    print(f"Used {cur_idx} to position {other_idx} at {r_pt(other_in_s0)} ({len(positions)}/{len(sensors)} done)")
    return positions

def write_alignments(alignments, f):
    for idx, (rot, s_in_s0) in alignments.items():
        rots = ",".join(str(r) for r in rot.flat)
        pos = ",".join(str(r) for r in s_in_s0)
        f.write(f"{idx},{rots},{pos}\n")

def read_alignments(s):
    lines = (l.strip() for l in s.splitlines())
    for line in lines:
        words = [int(w) for w in line.split(",")]
        idx = words[0]
        rot = np.array(words[1:10], dtype=np.int32)
        rot = rot.reshape((3, 3))
        pos = np.array(words[10:13], dtype=np.int32)
        pos = pos.reshape((3,))
        yield int(idx), (rot, pos)

def apply_alignments(alignments, sensors):
    for sensor_idx in range(len(sensors)):
        sensor = sensors[sensor_idx]
        sensor_rot, sensor_in_s0 = alignments[sensor_idx]
        sensor_len = sensor.shape[1]
        sensors[sensor_idx] = sensor_rot.dot(sensors[sensor_idx])
        add = np.tile(sensor_in_s0, (sensor_len, 1))
        sensors[sensor_idx] = sensors[sensor_idx] + add.T

def main():
    ex1 = load_problem(pathlib.Path('../input.txt').read_text())

    # s1_rot, s1_in_s0 = overlap(ex1[0], ex1[1])
    # ex1[1] = s1_rot.dot(ex1[1])
    # s4_rot, s4_in_s1 = overlap(ex1[1], ex1[4])
    # ex1[4] = s4_rot.dot(ex1[4])
    # s2_rot, s2_in_s4 = overlap(ex1[4], ex1[2])
    # ex1[2] = s2_rot.dot(ex1[2])
    # s3_rot, s3_in_s1 = overlap(ex1[1], ex1[3])
    # ex1[3] = s3_rot.dot(ex1[3])
    # assert_vec_eq(s1_in_s0, np.array([68,-1246,-43]))
    # assert_vec_eq(s4_in_s1 + s1_in_s0, np.array([-20,-1133,1061]))
    # assert_vec_eq(s2_in_s4 + s4_in_s1 + s1_in_s0, np.array([1105,-1205,1229]))
    # assert_vec_eq(s3_in_s1 + s1_in_s0, np.array([-92,-2380,-20]))

    # print("running...")
    # alignments = slam(ex1)
    # print("solved")

    # with pathlib.Path("alignments/input.csv").open(mode="wt") as f:
    #     write_alignments(alignments, f)

    alignments = dict(read_alignments(pathlib.Path("alignments/input.csv").read_text()))
    apply_alignments(alignments, ex1)

    all_pts = set(tuple(v) for v in it.chain.from_iterable(m.T for m in ex1))
    print(len(all_pts))

    max_dist = 0
    for (_, s1_in_s0), (_, s2_in_s0) in it.product(alignments.values(), alignments.values()):
        s2_in_s1 = s2_in_s0 - s1_in_s0
        print(r_pt(s1_in_s0), r_pt(s2_in_s0), r_pt(s2_in_s1), sum(abs(s2_in_s1)))
        max_dist = max(max_dist, sum(abs(s2_in_s1)))
    print(max_dist)

    # 10790 is too low >_<
