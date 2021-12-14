#!/usr/bin/perl

use strict;
use warnings;

use List::MoreUtils qw(minmax pairwise);
use List::Util qw(head tail reduce all sum uniq pairmap);
use Data::Dumper;

sub pairify {
    my @s = @_;
    my %output;
    map { $output{$s[$_] . $s[$_+1]} += 1; } (0 .. @s-2);
    return \%output;
}

sub expand {
    my ($pairs, $recipes) = @_;
    my $next_pairs = {};
    while (my ($k, $c) = each %$pairs) {
	my $rep = $recipes->{$k};
	my ($left, $right) = split(//, $k);
	$next_pairs->{$left . $rep} += $c;
	$next_pairs->{$rep . $right} += $c;
    }
    return $next_pairs;
}

sub get {
    my ($h, $i, $d) = @_;
    return exists($h->{$i}) ? $h->{$i} : $d;
}

sub arr_eq {
    for my $key (map { keys %$_ } @_) {
	return 0 if (length(uniq(map { get($a, $key, 0) } @_)) != 1);
    }
    return 1;
}

sub characters {
    my $pairs = shift;
    my %result = map { $_ => 1 } @{shift()};
    while (my ($key, $value) = each %$pairs) {
	foreach (split(//, $key)) {
	    $result{$_} += $value;
	};
    }
    return {pairmap { $a => $b/2 } %result};
}

sub total_characters {
    return sum values %{characters(@_)};
}

sub ingest {
    open my $f, shift or die;
    my @data = head(-1, split(//, <$f>));
    <$f>;
    my %recipes = map { /(..) -> (.)/ } <$f>;
    return (pairify(@data), [head(1, @data), tail(1, @data)], \%recipes);
}

sub analyze {
    my ($pairs, $endpoints) = @_;
    my $chars = characters($pairs, $endpoints);
    my ($mincount, $maxcount) = minmax(values(%$chars));
    return ($mincount, $maxcount, $maxcount - $mincount, sum(values(%$chars)));
}

# Ingest and test the example
my ($example_pairs, $example_endpoints, $example_recipes) = ingest('../example.txt');

$example_pairs = expand $example_pairs, $example_recipes;
die unless arr_eq(pairify(split(//, "NCNBCHB")), $example_pairs);

$example_pairs = expand $example_pairs, $example_recipes;
die unless arr_eq(pairify(split(//, "NBCCNBBBCBHCB")), $example_pairs);

$example_pairs = expand $example_pairs, $example_recipes;
die unless arr_eq(pairify(split(//, "NBBBCNCCNBBNBNBBCHBHHBCHB")), $example_pairs);

$example_pairs = expand $example_pairs, $example_recipes;
die unless arr_eq(pairify(split(//, "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")), $example_pairs);

$example_pairs = expand $example_pairs, $example_recipes;
die unless total_characters($example_pairs, $example_endpoints) == 97;

for (6 .. 10) { $example_pairs = expand $example_pairs, $example_recipes; }
my (undef, undef, $example_spread_10, $example_total_10) = analyze($example_pairs, $example_endpoints);
die unless $example_total_10 == 3073;
die unless $example_spread_10 == 1588;

for (11 .. 40) { $example_pairs = expand $example_pairs, $example_recipes; }
my ($example_min_40, $example_max_40, $example_spread_40, undef) = analyze($example_pairs, $example_endpoints);
die unless $example_max_40 == 2192039569602;
die unless $example_min_40 == 3849876073;
die unless $example_spread_40 == 2188189693529;

# Now do the actual input
my ($input_pairs, $input_endpoints, $input_recipes) = ingest('../input.txt');
for (1..10) { $input_pairs = expand $input_pairs, $input_recipes; }
print Dumper [analyze($input_pairs, $input_endpoints)];
for (11 .. 40) { $input_pairs = expand $input_pairs, $input_recipes; }
print Dumper [analyze($input_pairs, $input_endpoints)];
