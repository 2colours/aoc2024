#!/usr/bin/env raku

use experimental :cached;

my @starter = 'input.txt'
	.IO
	.slurp
	.chomp
	.split(' ');

sub iterate(Int() $_) is cached {
	when 0 { 1 }
	when .chars %% 2 { .comb.batch(.chars div 2).map(*.join.Int).Slip }
	default { $_ * 2024 }
}

sub how-many-values($from, $blinks) is cached {
	when $blinks == 0 { 1 }
	my @new-values = iterate($from);
	@new-values.map(*.&how-many-values($blinks - 1)).sum
}

say (join ' ', @starter.map(*.&how-many-values($_)).sum for (25, 75));
