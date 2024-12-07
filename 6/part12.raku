#!/usr/bin/env raku
use v6.e.PREVIEW;
use ValueList;

enum Direction<Left Right Up Down>;
enum PathResult<RunsOut Loops Continues>;

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

sub walk-path(@table is readonly, $cursor is rw, $direction is rw, %walked --> PathResult) {
	my $result = RunsOut;
	my $dimensions = (@table.elems, @table[0].elems);
	for path-indices($dimensions, $cursor, $direction) -> $position {
		if @table[||$position] eq '#' {
			$result = Continues;
			last;
		}
		$cursor = $position;
		if %walked{$direction}{ValueList($position)} {
			return Loops;
		} else {
			%walked{$direction}{ValueList($position)} = True; # Horrible design choice of https://docs.raku.org/type/SetHash#method_set forces me to use this hack
		}
	}
	$direction.=&rotate;
	$result
}

sub simulate-traversal(@table, $starting-pos, $starting-direction, :$loop-detection-only) {
	my $cursor = $starting-pos;
	my $direction = $starting-direction;
	my %walked = ($_ => SetHash.new for Direction.keys);
	my $walk-result;
	repeat {
		$walk-result = walk-path(@table, $cursor, $direction, %walked);
	} until $walk-result != Continues;
	$loop-detection-only ?? $walk-result == Loops !! %walked.values.reduce(&[(|)])
}	

my @table = 'input.txt'.IO.lines.map(*.comb.Array);
my $starting-pos = @table.map(*.first(:k, '^')).first(*.defined, :kv);
my $walked-fields = simulate-traversal(@table, $starting-pos, Up);
say $walked-fields.elems;
my $fix-count = 0;
for $walked-fields.keys {
	next if $_ eqv ValueList($starting-pos);
	@table[||List($_)] = '#';
	$fix-count++ if simulate-traversal(@table, $starting-pos, Up, :loop-detection-only);
	@table[||List($_)] = '.';
}
say "$fix-count";
