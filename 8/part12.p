#!/usr/bin/env prolog

:- use_module(library(clpfd)).

:- initialization main.

:- dynamic matrix/2.

build_table(T) :- build_table(T, 1).
build_table([], _Index).
build_table([THD|TTL], Current_Index) :- build_row(THD, Current_Index, 1), Next_Index is Current_Index + 1, build_table(TTL, Next_Index).
build_row([], _Row, _Column).
build_row([RHD|RTL], Row, Column) :- asserta(matrix(Row-Column, RHD)), Next_Column is Column + 1, build_row(RTL, Row, Next_Column).

dimensions(Table, Max_Rows-Max_Columns) :-
        length(Table, Max_Rows),
        (
                Table = [THD|_TTL] -> length(THD, Max_Columns);
                Max_Columns = 0
        ).

antinode_part1(Max_Row-Max_Column, (RA1-CA1)-(RA2-CA2), Antinode_Row-Antinode_Column) :-
        Antinode_Row in 1 .. Max_Row,
        Antinode_Column in 1 .. Max_Column,
        RA1 + Row_Difference #= RA2,
        CA1 + Column_Difference #= CA2,
        (
                RA1 + 2 * Row_Difference #= Antinode_Row, CA1 + 2 * Column_Difference #= Antinode_Column ;
                RA1 - Row_Difference #= Antinode_Row, CA1 - Column_Difference #= Antinode_Column
        ).

antinode_part2(Max_Row-Max_Column, (RA1-CA1)-(RA2-CA2), Antinode_Row-Antinode_Column) :-
        Antinode_Row in 1 .. Max_Row,
        Antinode_Column in 1 .. Max_Column,
        RA1 + Row_Difference #= RA2,
        CA1 + Column_Difference #= CA2,
        GCD is gcd(Row_Difference, Column_Difference), % it gave the right result without making the components relative primes as well but I think that's just fortunate input
        Row_Delta #= div(Row_Difference, GCD),
        Column_Delta #= div(Column_Difference, GCD),
        RA1 + N * Row_Delta #= Antinode_Row,
        CA1 + N * Column_Delta #= Antinode_Column,
        indomain(N).

find_antinodes(Boundaries, Part, Solution) :-
        findall(Antinode_Pos, (
                Code in 0'0 .. 0'9 \/ 0'A .. 0'Z \/ 0'a .. 0'z,
                matrix(Pos1, Code),
                matrix(Pos2, Code),
                Pos1 @< Pos2,
                atom_concat(antinode_, Part, Finder),
                call(Finder, Boundaries, Pos1-Pos2, Antinode_Pos)
                ), Positions_Redundant),
        list_to_ord_set(Positions_Redundant, Positions),
        length(Positions, Solution).

main :-
        csv_read_file("input.txt", Rows),
        maplist([row(Atom), Codes]>>atom_codes(Atom, Codes), Rows, Table),
        build_table(Table),
        dimensions(Table, Boundaries),
        compile_predicates([matrix/2]),
        maplist([Part, Solution]>>find_antinodes(Boundaries, Part, Solution), [part1, part2], Solutions),
        format("~d ~d", Solutions),
        halt.
