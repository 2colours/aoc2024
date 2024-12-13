#!/usr/bin/env prolog

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

:- initialization main.

button_line(button(BType, XVal, YVal)) --> "Button ", [BType_Raw], { atom_char(BType, BType_Raw) }, ": X+", integer(XVal), ", Y+", integer(YVal).

prize_line(prize(XVal, YVal)) --> "Prize: X=", integer(XVal), ", Y=", integer(YVal).

machine_lines([[XA,YA], [XB,YB], [XT,YT]]) --> button_line(button('A', XA, YA)), "\n", button_line(button('B', XB, YB)), "\n", prize_line(prize(XT, YT)).

day13_format(Machines) --> sequence(machine_lines, "\n\n", Machines), "\n".

solve_machine([A_params, B_params, T_params], Target_Offset, A, B) :-
        [A, B] ins 0..sup,
        maplist({A,B}/[A_param, B_param, T_param]>>(
                        A * A_param + B * B_param #= Target_Offset + T_param
                ), A_params, B_params, T_params),
        once(labeling([max(3 * A + B)], [A, B])).

machine_cost(Part, M, Cost) :-
        (Part = part1 ->
                Target_Offset = 0
        ;
        Part = part2 ->
                Target_Offset = 10000000000000
        ),
        (solve_machine(M, Target_Offset, A, B) ->
                Cost is 3 * A + B
        ;
                Cost = 0
        ).

main :-
        phrase_from_file(day13_format(All_Machines), "input.txt"),
        maplist([Part, Solution]>>(
                        maplist(machine_cost(Part), All_Machines, All_Costs),
                        sum_list(All_Costs, Solution)
                ), [part1, part2], Solutions),
        format("~d ~d", Solutions),
        halt.
