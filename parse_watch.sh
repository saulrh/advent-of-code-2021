#!/bin/bash

fd --exact-depth 2 --extension txt --extension lark | entr -c ./parse.sh
