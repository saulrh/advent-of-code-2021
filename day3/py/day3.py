#!/usr/bin/env python3

import sys
import rich
from rich import print
import collections

def parse_line(line):
    return [c == "1" for c in line.strip()]

def to_int(line):
    return sum(2**idx for idx, b in enumerate(reversed(line)) if b)

assert to_int([True]) == 1
assert to_int([False]) == 0
assert to_int([True, False]) == 2
assert to_int([True, True]) == 3
assert to_int([True, True, True]) == 7


def counts(data):
    width = len(data[0])
    result = [0] * width
    for line in data:
        for idx, char in enumerate(line):
            result[idx] += 1 if char else 0
    return result

def commons(data):
    length = len(data)
    threshold = length / 2
    result = []
    for count in counts(data):
        if count == threshold:
            result.append(None)
        else:
            result.append(count > threshold)
    return result
    

def gamma_epsilon(data):
    common = commons(data)
    width = len(common)
    gamma = 0
    epsilon = 0
    for idx in range(width):
        part = 2**(width - idx - 1)
        if common[idx]:
            gamma += part
        else:
            epsilon += part
    return gamma, epsilon

def oxygen_pred(line, idx, count):
    if count[True] == count[False]:
        return line[idx]
    elif count[True] > count[False]:
        return line[idx]
    else:
        return not line[idx]

def co2_pred(line, idx, count):
    return not oxygen_pred(line, idx, count)

def compute_oxygen(data):
    idx = 0
    while len(data) > 1:
        count = collections.Counter(l[idx] for l in data)
        data = [d for d in data if oxygen_pred(d, idx, count)]
        idx += 1
    assert len(data) == 1
    return to_int(data[0])

def compute_co2(data):
    idx = 0
    while len(data) > 1:
        count = collections.Counter(l[idx] for l in data)
        data = [d for d in data if co2_pred(d, idx, count)]
        idx += 1
    assert len(data) == 1
    return to_int(data[0])

with open("data/day3_example.txt", 'r') as f:
    example = [parse_line(l) for l in f.readlines()]

gamma_example, epsilon_example = gamma_epsilon(example)
print(gamma_example)
print(epsilon_example)
assert gamma_example == 22
assert epsilon_example == 9
oxygen_example = compute_oxygen(example)
print(oxygen_example)
assert oxygen_example == 23
co2_example = compute_co2(example)
print(co2_example)
assert co2_example == 10

with open(sys.argv[1], 'r') as f:
    data = [parse_line(l) for l in f.readlines()]

width = len(data[0])
assert all(len(l) == width for l in data)

gamma, epsilon = gamma_epsilon(data)
print(gamma, epsilon)
print(gamma * epsilon)
oxygen = compute_oxygen(data)
print(oxygen)
co2 = compute_co2(data)
assert oxygen * co2 > 851
print(co2)
print(oxygen * co2)
