#!/usr/bin/env raku

my ($left, $right) = 'input.txt'.IO.lines>>.words.&zip;
$left.map({ $_ * $right.grep($_) }).sum.say;
