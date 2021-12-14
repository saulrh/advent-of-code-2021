#!/usr/bin/perl

use strict;
use warnings;

use List::Util qw(min max);

my $data = <>;
<>;
my @recipes = map { [/(.)(.) -> (.)/] } <>;

for (1..10) {
    foreach (@recipes) {
	my ($l, $r, $b) = @$_;
	$b = lc $b;
	$data =~ s/$l$r/$l$b$r/g;
	$data =~ s/$l$r/$l$b$r/g;
    }
    $data = uc $data;
}
my @counts = map { $data =~ s/$_//ig } qw(b n c h);
print max(@counts) - min(@counts), "\n";
