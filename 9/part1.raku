#!/usr/bin/env raku

my @disk = 'input.txt'
	.IO
	.slurp
	.comb
	.map(-> $filled, $empty { my $id = $++; slip(flat($id xx $filled, '.' xx $empty)) });
my @positions-to-fill = @disk.grep(:k, '.');
for @positions-to-fill -> $position-filled {
	my $position-moved = @disk.first(:k, :end, * ne '.');
	last if $position-filled > $position-moved;
	say $position-filled, $position-moved;
	@disk[$position-filled] = @disk[$position-moved];
	@disk[$position-moved] = '.';
}
say "Result:";
@disk.toggle(* ne '.').map({ $++ * $_ }).sum.say;
