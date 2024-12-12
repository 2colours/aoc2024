#!/usr/bin/env raku

my @starter = 'input.txt'
	.IO
	.slurp
	.chomp
	.split(' ');

sub iterate(Int() $_) {
	when 0 { 1 }
	when .chars %% 2 { .comb.batch(.chars div 2).map(*.join.Int).Slip }
	default { $_ * 2024 }
}

my @current = @starter;
@current.=map(&iterate) for ^25;
say @current.elems;
