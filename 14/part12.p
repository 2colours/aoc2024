#!/usr/bin/env prolog

:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

:- initialization main.

robot_line([[Start_X, Start_Y], [Dir_X, Dir_Y]]) --> "p=", integer(Start_X), ",", integer(Start_Y), " v=", integer(Dir_X), ",", integer(Dir_Y), "\n".

day14_format([]) --> [].
day14_format([Robot|Robots_Rest]) --> robot_line(Robot), day14_format(Robots_Rest).

robot_stepped([Start, Direction], Dimensions, Steps, End) :-
        maplist([S, Dir, Dim, Res]>>(Res is (S + Steps * Dir) mod Dim), Start, Direction, Dimensions, End).

empty_quadrant_assoc(A) :-
        findall(Quadrant_Pos, (length(Quadrant_Pos, 2), maplist(between(-1, 1), Quadrant_Pos)), Keys),
        maplist([Key, Key-0]>>true, Keys, List),
        list_to_assoc(List, A).

positions_after_steps(Robots, Dimensions, Steps, Positions) :-
        maplist([Robot, New_Pos]>>robot_stepped(Robot, Dimensions, Steps, New_Pos), Robots, Positions).

positions_quadrants(Dimensions, Positions, Quadrants, Exclusion_Info) :-
        empty_quadrant_assoc(Starter_Assoc),
        foldl([Pos, Assoc_Before, Assoc_After]>>(
                maplist([Coordinate, Dimension, Direction_From_Middle]>>(Direction_From_Middle is cmpr(Coordinate, (Dimension - 1) / 2)), Pos, Dimensions, Category),
                get_assoc(Category, Assoc_Before, Category_Count_Before, Assoc_After, Category_Count_After),
                Category_Count_After is Category_Count_Before + 1
                ), Positions, Starter_Assoc, Compared_To_Mid_Count),
        relevant_quadrant(Compared_To_Mid_Count, Quadrants, Exclusion_Info).

pos_excluded(Pos, Exclusion_Info) :-
        (
                member(vertical, Exclusion_Info),
                Pos = [0, _V]
        ;
                member(horizontal, Exclusion_Info),
                Pos = [_V, 0]
        ).

relevant_quadrant(Assoc_In, Assoc_Accepted, Exclusion_Info) :-
        assoc_to_list(Assoc_In, List_In),
        exclude([Key-_Value]>>pos_excluded(Key, Exclusion_Info), List_In, List_Accepted),
        list_to_assoc(List_Accepted, Assoc_Accepted).

draw(Dim, R) :- draw(Dim, R, [0, 0]).
draw([_, Dy], _, [_, Dy]) :- put('\n'), put('\n'), !.
draw([Dx, Dy], R, [Dx, Y]) :- Y1 is Y + 1, put('\n'), draw([Dx, Dy], R, [0, Y1]), !.
draw([Dx, Dy], R, [X, Y]) :-
    ( member([X, Y], R) -> put('#') ; put('.') ),
    X1 is X + 1,
    draw([Dx, Dy], R, [X1, Y]), !.

product(Nums, Prod) :-
        foldl([Current, Prod_Before, Prod_After]>>(Prod_After is Prod_Before * Current), Nums, 1, Prod).

part1(All_Robots, Dimensions, Solution) :-
        positions_after_steps(All_Robots, Dimensions, 100, All_Positions),
        draw(Dimensions, All_Positions),
        positions_quadrants(Dimensions, All_Positions, Quadrants, [horizontal, vertical]),
        assoc_to_values(Quadrants, Quadrant_Counts),
        product(Quadrant_Counts, Solution).

part2(All_Robots, Dimensions, Solution) :-
        product(Dimensions, Full_Period),
        Steps in 1 .. Full_Period,
        indomain(Steps),
        positions_after_steps(All_Robots, Dimensions, Steps, All_Positions),
        reasonable_candidate(All_Positions),
        format("After ~d steps:\n", [Steps]),
        draw(Dimensions, All_Positions),
        Solution = Steps.

reasonable_candidate(Positions) :- %there is a frame no smaller than 10x10... I hope the author has a nice place in hell
        member([X1, Y1], Positions),
        forall(between(1, 9, Offset), (Y_Offset is Y1 + Offset, X_Offset is X1 + Offset, memberchk([X1, Y_Offset], Positions), memberchk([X_Offset, Y1], Positions))).

main :-
        phrase_from_file(day14_format(All_Robots), "input.txt"),
        Dimensions = [101, 103],
        part1(All_Robots, Dimensions, Solution1),
        write(Solution1), write("\n"),
        part2(All_Robots, Dimensions, Solution2),
        write(Solution2),
        halt.
