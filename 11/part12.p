#!/usr/bin/env prolog

:- initialization main.

:- table next_values/2, how_many_produced/3.

next_values(0, [1]) :- !.
next_values(Even_Length_Value, Two_Halves) :-
        number_chars(Even_Length_Value, Even_Length_Chars),
        length(Even_Length_Chars, L),
        divmod(L, 2, Half_L, 0), !,
        maplist([List]>>length(List, Half_L), [First_Half_Chars, Second_Half_Chars]),
        prefix(First_Half_Chars, Even_Length_Chars),
        append(First_Half_Chars, Second_Half_Chars, Even_Length_Chars),
        maplist(number_chars, Two_Halves, [First_Half_Chars, Second_Half_Chars]).
next_values(Value, [New_Value]) :- New_Value is Value * 2024, !.

how_many_produced(_Starter, 0, 1) :- !.
how_many_produced(Starter, Blinks, Result) :-
        next_values(Starter, New_Values),
        Blinks_Remaining is Blinks - 1,
        maplist([S, R]>>how_many_produced(S, Blinks_Remaining, R), New_Values, New_Produced),
        sum_list(New_Produced, Result).




main :-
        csv_read_file("input.txt", [Row], [separator(0' )]),
        Row =.. [row | Starting_Values],
        maplist([Blinks, Solution]>>(
                maplist([Starter, Result]>>how_many_produced(Starter, Blinks, Result), Starting_Values, Solution_Per_Slot),
                sum_list(Solution_Per_Slot, Solution)
                ), [25, 75], Solutions),
        format("~d ~d", Solutions),
        halt.
