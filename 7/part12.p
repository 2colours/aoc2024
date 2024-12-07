#!/usr/bin/env prolog

:- initialization main.

process_file(File_Name, Equation_Pairs) :-
        csv_read_file(File_Name, Rows, [match_arity(false), separator(0' )]),
        maplist([Tuple, Goal_Result-Operands]>>(
                Tuple =.. [row, Goal_Raw | Operands],
                atom_concat(Goal_Atom, ':', Goal_Raw),
                atom_number(Goal_Atom, Goal_Result)
                ), Rows, Equation_Pairs).

evaluated_operation(Op1, Op2, Result, _Part) :- Result is Op1 * Op2 ; Result is Op1 + Op2.
evaluated_operation(Op1, Op2, Result, part2) :- atom_concat(Op1, Op2, Result_Atom), atom_number(Result_Atom, Result).

arithmetic_test(Expected, Acc, [], _Part) :- Expected = Acc.
arithmetic_test(Expected, Acc, [VH|VT], Part) :- Acc =< Expected, evaluated_operation(Acc, VH, New_Acc, Part), arithmetic_test(Expected, New_Acc, VT, Part). % Acc =< Expected is for optimization
arithmetic_test(Expected, [VH|VT], Part) :- arithmetic_test(Expected, VH, VT, Part).

main :-
        process_file("input.txt", Equation_Pairs),
        maplist([Part, Solution]>>(
                include([Target_Value-Operands]>>arithmetic_test(Target_Value, Operands, Part), Equation_Pairs, Good_Equations),
                maplist([Target_Value, Target_Value-Operands]>>true, Good_Values, Good_Equations),
                sum_list(Good_Values, Solution)
        ), [part1, part2], Solutions),
        format("~d ~d", Solutions),
        halt.
