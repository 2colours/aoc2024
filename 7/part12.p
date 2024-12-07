#!/usr/bin/env prolog

:- initialization main.

process_file(File_Name, Equation_Pairs) :-
        csv_read_file(File_Name, Rows, [match_arity(false), separator(0' )]),
        maplist([Tuple, Goal_Result-Operands]>>(
                Tuple =.. [row, Goal_Raw | Operands],
                atom_concat(Goal_Atom, ':', Goal_Raw),
                atom_number(Goal_Atom, Goal_Result)
                ), Rows, Equation_Pairs).

arithmetic_expr(A, B, A+B).
arithmetic_expr(A, B, A*B).

arithmetic_test(Expected, Acc, []) :- Expected = Acc.
arithmetic_test(Expected, Acc, [VH|VT]) :- Acc =< Expected, arithmetic_expr(Acc, VH, Expr), New_Acc is Expr, arithmetic_test(Expected, New_Acc, VT). % Acc =< Expected is for optimization
arithmetic_test(Expected, [VH|VT]) :- arithmetic_test(Expected, VH, VT).

main :-
        process_file("input.txt", Equation_Pairs),
        include([Target_Value-Operands]>>arithmetic_test(Target_Value, Operands), Equation_Pairs, Good_Equations),
        maplist([Target_Value, Target_Value-Operands]>>true, Good_Values, Good_Equations),
        sum_list(Good_Values, Solution1),
        print(Solution1).
