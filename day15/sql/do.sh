#!/usr/bin/bash

sqlite3 data.db <load.sql
sqlite3 data.db <part2.sql
sqlite3 data.db <part1_1.sql

width=$(echo "select max(r) + max(c) + 10 from risks" | sqlite3 data.db)

for i in $(seq $width)
do
    echo "Iteration $i/$width..."
    sqlite3 data.db <part1_2.sql
done

./display.py
sqlite3 data.db <ans.sql
