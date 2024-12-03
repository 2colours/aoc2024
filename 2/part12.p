#!/usr/bin/env prolog

:- initialization main.

safe_values(List, _Direction) :- length(List, L), L < 2, !.
safe_values([First, Second | Rest], Direction) :-
        (First < Second, Direction \= desc, New_Direction = asc ; First > Second, Direction \= asc, New_Direction = desc),
        Distance is abs(First - Second),
        between(1, 3, Distance),
        safe_values([Second | Rest], New_Direction).

main :-
        csv_read_file("input.txt", Rows, [separator(0' ), match_arity(false)]),
        maplist([Row, List]>>(Row =.. [row | List]), Rows, Rows_Lists),
        include([Row]>>(select(_V, Row, Relaxed_Row), safe_values(Relaxed_Row, any)), Rows_Lists, Safe_Rows),
        length(Safe_Rows, Solution),
        write(Solution).
