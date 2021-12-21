#!/usr/bin/env bash

echo "By day:"
fd --type directory --exact-depth 2 | sed -e 's|/|\t|' | sort -n | column --table


echo "By solution:"
fd --type directory --exact-depth 2 | sed -e 's|/|\t|' | awk '{ print $2 " " $1}' | sort -n | column --table
