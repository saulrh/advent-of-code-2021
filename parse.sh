#!/bin/bash

for f in day*/*.txt
do
    aoc_parser --data=$f
done
