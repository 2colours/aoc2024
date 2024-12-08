#!/usr/bin/env prolog

:- use_module(library(clpfd)).

:- initialization main.

:- dynamic matrix/2.

build_table(T) :- build_table(T, 0).
build_table([], _Index).
build_table([THD|TTL], Current_Index) :- build_row(THD, Current_Index, 0), Next_Index is Current_Index + 1, build_table(TTL, Next_Index).
build_row([], _Row, _Column).
build_row([RHD|RTL], Row, Column) :- asserta(matrix(Row-Column, RHD)), Next_Column is Column + 1, build_row(RTL, Row, Next_Column).

antinode((RA1-CA1)-(RA2-CA2), Antinode_Row-Antinode_Column) :-
        RA1 + Row_Difference #= RA2,
        CA1 + Column_Difference #= CA2,
        (
                RA1 + 2 * Row_Difference #= Antinode_Row, CA1 + 2 * Column_Difference #= Antinode_Column ;
                RA1 - Row_Difference #= Antinode_Row, CA1 - Column_Difference #= Antinode_Column
        ),
        matrix(Antinode_Row-Antinode_Column, _V).


main :-
        csv_read_file("input.txt", Rows),
        maplist([row(Atom), Codes]>>atom_codes(Atom, Codes), Rows, Table),
        build_table(Table),
        compile_predicates([matrix/2]),
        findall(Antinode_Pos, (
                Code in 0'0 .. 0'9 \/ 0'A .. 0'Z \/ 0'a .. 0'z,
                matrix(Pos1, Code),
                matrix(Pos2, Code),
                Pos1 @< Pos2,
                antinode(Pos1-Pos2, Antinode_Pos)
                ), Antinode_Positions),
        list_to_ord_set(Antinode_Positions, Antinode_Positions_Set),
        length(Antinode_Positions_Set, Solution1_Right),
        format("~d", [Solution1_Right]),
        halt.
