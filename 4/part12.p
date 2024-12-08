#!/usr/bin/env prolog

:- initialization main.

:- dynamic matrix/2.

build_table(T) :- build_table(T, 0).
build_table([], _Index).
build_table([THD|TTL], Current_Index) :- build_row(THD, Current_Index, 0), Next_Index is Current_Index + 1, build_table(TTL, Next_Index).
build_row([], _Row, _Column).
build_row([RHD|RTL], Row, Column) :- asserta(matrix(Row-Column, RHD)), Next_Column is Column + 1, build_row(RTL, Row, Next_Column).

direction(DRow-DColumn) :- between(-1, 1, DRow), between(-1, 1, DColumn), \+ (DRow = 0, DColumn = 0).
diagonal_direction(DRow-DColumn) :- member(DRow, [-1, 1]), member(DColumn, [-1, 1]).
adjacent(Row1-Column1, Row2-Column2, DRow-DColumn) :- plus(DRow, Row2, Row1), plus(DColumn, Column2, Column1).

find_word([], []).
find_word([WHD|WTL], [PHD|PTL]) :- matrix(PHD, WHD), direction(Dir), find_word([WHD|WTL], [PHD|PTL], Dir).
find_word([Word_Atom], [Position], _Dir) :- matrix(Position, Word_Atom).
find_word([WA1, WA2 |WTL], [P1, P2 |PTL], Direction) :- matrix(P1, WA1), adjacent(P1, P2, Direction), matrix(P2, WA2), find_word([WA2|WTL], [P2|PTL], Direction).

main :-
        csv_read_file("input.txt", Rows),
        maplist([row(Atom), Chars]>>atom_chars(Atom, Chars), Rows, Table),
        build_table(Table),
        compile_predicates([matrix/2]),
        findall(P, find_word(['X', 'M', 'A', 'S'], P), Ps),
        length(Ps, Solution1),
        writeln(Solution1),
        findall(PC, (diagonal_direction(D1), find_word(['M', 'A', 'S'], [_PM1, PC, _PS1], D1), diagonal_direction(D2), D1 @< D2, find_word(['M', 'A', 'S'], [_PM2, PC, _PS2], D2)), PCs),
        length(PCs, Solution2),
        write(Solution2).
