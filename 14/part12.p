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

main :-
        phrase_from_file(day14_format(All_Robots), "input.txt"),
        Dimensions = [101, 103],
        maplist([Robot, New_Pos]>>robot_stepped(Robot, Dimensions, 100, New_Pos), All_Robots, All_Positions),
        empty_quadrant_assoc(Starter_Assoc),
        foldl([Pos, Assoc_Before, Assoc_After]>>(
                maplist([Coordinate, Dimension, Direction_From_Middle]>>(Direction_From_Middle is cmpr(Coordinate, (Dimension - 1) / 2)), Pos, Dimensions, Category),
                get_assoc(Category, Assoc_Before, Category_Count_Before, Assoc_After, Category_Count_After),
                Category_Count_After is Category_Count_Before + 1
                ), All_Positions, Starter_Assoc, Quadrant_Grouped),
        assoc_to_list(Quadrant_Grouped, Quadrant_Entries),
        include([Key-_Value]>>maplist(\=(0), Key), Quadrant_Entries, Quadrant_No_Mid),
        pairs_values(Quadrant_No_Mid, Quadrant_Counts),
        foldl([Current, Prod_Before, Prod_After]>>(Prod_After is Prod_Before * Current), Quadrant_Counts, 1, Solution1),
        write(Solution1),
%        maplist([Part, Solution]>>(
%                        maplist(machine_cost(Part), All_Machines, All_Costs),
%                        sum_list(All_Costs, Solution)
%                ), [part1, part2], Solutions),
%        format("~d ~d", Solutions),
        halt.
