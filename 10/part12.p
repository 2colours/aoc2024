#!/usr/bin/env prolog

:- use_module(library(clpfd)).

:- initialization main.

:- dynamic matrix/2.

build_table(T) :- build_table(T, 1).
build_table([], _Index).
build_table([THD|TTL], Current_Index) :- build_row(THD, Current_Index, 1), Next_Index is Current_Index + 1, build_table(TTL, Next_Index).
build_row([], _Row, _Column).
build_row([RHD|RTL], Row, Column) :- asserta(matrix(Row-Column, RHD)), Next_Column is Column + 1, build_row(RTL, Row, Next_Column).


direction(0-DColumn) :- member(DColumn, [1, -1]).
direction(DRow-0) :- member(DRow, [1, -1]).
adjacent(Row1-Column1, Row2-Column2, DRow-DColumn) :- plus(DRow, Row2, Row1), plus(DColumn, Column2, Column1).

hiking_path(From_Value-From_Value, From_Position-From_Position) :-
        matrix(From_Position, From_Value).
hiking_path(From_Value-To_Value, From_Position-To_Position) :-
        matrix(From_Position, From_Value),
        direction(Dir),
        adjacent(Next_Position, From_Position, Dir),
        Next_Value is From_Value + 1,
        hiking_path(Next_Value-To_Value, Next_Position-To_Position).

main :-
        csv_read_file("input.txt", Rows, [convert(false)]),
        maplist([row(Atom), Heights]>>(atom_chars(Atom, Chars), maplist(atom_number, Chars, Heights)), Rows, Table),
        build_table(Table),      
        findall(Endpoints, hiking_path(0-9, Endpoints), Endpoints_All_Routes),
        list_to_ord_set(Endpoints_All_Routes, Endpoints_Unique),
        maplist([Endpoints_List, Solution]>>(
                group_pairs_by_key(Endpoints_List, Endpoints_By_Starting_Point),
                maplist([_Starting_Pos-Destinations, Reach_Count]>>length(Destinations, Reach_Count), Endpoints_By_Starting_Point, Reach_Counts),
                sum_list(Reach_Counts, Solution)
        ), [Endpoints_Unique, Endpoints_All_Routes], Solutions),
        format("~d ~d", Solutions),
        halt.
