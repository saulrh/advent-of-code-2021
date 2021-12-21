#!/usr/bin/env swipl

:- initialization(main, main).

:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(record)).
:- use_module(library(yall)).

eos([], []).

lines([]) --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

line([]) --> ("\n" ; call(eos)), !.
line([Bit|Bits]) --> [Char], {char_to_bit(Bit, Char)}, line(Bits).

char_to_bit(Bit, Code) :-
    char_code(Char, Code),
    number_string(Bit, [Char]).

bits_in_column(Numbers, Column, Bits) :-
    maplist(nth1(Column), Numbers, Bits).

pair_le_by_second([_-Av], [_-Bv]) :-
    Av =< Bv.

most_common_bit(Numbers, Column, Bit) :-
    bits_in_column(Numbers, Column, Bits),
    msort(Bits, SortedBits),
    clumped(SortedBits, Counts),
    transpose_pairs(Counts, CountsT),
    max_member(_-Bit, CountsT).

least_common_bit(Numbers, Column, Bit) :-
    bits_in_column(Numbers, Column, Bits),
    msort(Bits, SortedBits),
    clumped(SortedBits, Counts),
    transpose_pairs(Counts, CountsT),
    min_member(_-Bit, CountsT).

columns([Number|_], Columns) :-
    proper_length(Number, NumColumns),
    numlist(1, NumColumns, Columns).

gamma(Numbers, Gamma) :-
    columns(Numbers, Columns),
    maplist(most_common_bit(Numbers), Columns, Bits),
    bits_to_number(Bits, Gamma).

epsilon(Numbers, Epsilon) :-
    columns(Numbers, Columns),
    maplist(least_common_bit(Numbers), Columns, Bits),
    bits_to_number(Bits, Epsilon).

bits_to_number(Bits, Number) :-
    bits_to_number_(Bits, 0, Number).

bits_to_number_([], Acc, Acc).

bits_to_number_([B|Bs], Acc, Number) :-
    (B #= 0 ; B #= 1),
    NextAcc #= (2 * Acc) + B,
    bits_to_number_(Bs, NextAcc, Number).

part1(Numbers, Result) :-
    gamma(Numbers, Gamma),
    epsilon(Numbers, Epsilon),
    Result #= Gamma * Epsilon.

bit_in_column_is(Column, MostCommonBit, Number) :-
    nth1(Column, Number, MostCommonBit).

oxygen(Numbers, Oxygen) :-
    oxygen_(Numbers, 1, Oxygen).

oxygen_([Number], _, Oxygen) :-
    debug(oxygen, "doing oxygen base case: ~q", [Number]),
    bits_to_number(Number, Oxygen).

oxygen_(Numbers, Column, Oxygen) :-
    debug(oxygen, "doing oxygen: ~q, ~d", [Numbers, Column]),
    most_common_bit(Numbers, Column, MostCommonBit),
    debug(oxygen, "Most common bit: ~D", [MostCommonBit]),
    include(bit_in_column_is(Column, MostCommonBit), Numbers, FilteredNumbers),
    NextColumn #= Column + 1,
    oxygen_(FilteredNumbers, NextColumn, Oxygen).

co2(Numbers, Co2) :-
    co2_(Numbers, 1, Co2).

co2_([Number], _, Co2) :-
    debug(co2, "doing co2 base case: ~q", [Number]),
    bits_to_number(Number, Co2).

co2_(Numbers, Column, Co2) :-
    debug(co2, "doing co2: ~q, ~d", [Numbers, Column]),
    least_common_bit(Numbers, Column, LeastCommonBit),
    debug(co2, "Least common bit: ~D", [LeastCommonBit]),
    include(bit_in_column_is(Column, LeastCommonBit), Numbers, FilteredNumbers),
    NextColumn #= Column + 1,
    co2_(FilteredNumbers, NextColumn, Co2).

part2(Numbers, Result) :-
    oxygen(Numbers, Oxygen),
    co2(Numbers, Co2),
    Result #= Oxygen * Co2.


main(_Argv) :-
    phrase_from_file(lines(Example), '../example.txt'),
    phrase_from_file(lines(Input), '../input.txt'),
    part1(Example, ExamplePart1),
    part1(Input, InputPart1),
    write("Example Part 1: "), write(ExamplePart1), nl,
    write("Input Part 1: "), write(InputPart1), nl,

    part2(Example, ExamplePart2),
    part2(Input, InputPart2),
    write("Example Part 1: "), write(ExamplePart2), nl,
    write("Input Part 1: "), write(InputPart2), nl,

    halt.
