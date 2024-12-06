#!/usr/bin/env raku
use v6.e.PREVIEW;

enum Direction<Left Right Up Down>;

multi rotate(Left --> Up) {}
multi rotate(Up --> Right) {}
multi rotate(Right --> Down) {}
multi rotate(Down --> Left) {}

multi path-indices($, ($row, $column), Left) {
	my @columns = $column ... 0;
	$row X @columns
}

multi path-indices(($, $width), ($row, $column), Right) {
	my @columns = $column ...^ $width;
	$row X @columns
}
		
multi path-indices($, ($row, $column), Up) {
	my @rows = $row ... 0;
	@rows X $column
}
		
multi path-indices(($height, $), ($row, $column), Down) {
	my @rows = $row ...^ $height;
	@rows X $column
}

sub walk-path(@table, $cursor is rw, $direction is rw --> Bool) {
	my $runs-out = True;
	my $dimensions = (@table.elems, @table[0].elems);
	for path-indices($dimensions, $cursor, $direction) -> $position {
		if @table[||$position] eq '#' {
			$runs-out = False;
			last;
		}
		$cursor = $position;
		@table[||$cursor] = 'X';
	}
	$direction.=&rotate;
	$runs-out
}

my @table = 'input.txt'.IO.lines.map(*.comb.Array);
my $starting-pos = @table.map(*.first(:k, '^')).first(*.defined, :kv);
@table[||$starting-pos] = '.';
my $cursor = $starting-pos;
my $direction = Up;
say "Starting spot: $cursor";
say "Starting direction: $direction";
until walk-path(@table, $cursor, $direction) {
}
@table>>.grep('X').sum.say

