#!/usr/bin/env prolog

:- initialization main.

process_input(Filename, Given_Rules, Sequences) :-
        read_file_to_string(Filename, Content, []),
        re_split("\n\n", Content, [Rule_Part, _Separator, Sequence_Part]),
        string_codes(Rule_Part, Rule_Part_Codes),
        phrase(csv(Csv_Rules, [match_arity(false), separator(0'|)]), Rule_Part_Codes),
        maplist([Tuple, List]>>(Tuple =.. [row | List]), Csv_Rules, Given_Rules),
        string_codes(Sequence_Part, Sequence_Part_Codes),
        phrase(csv(Csv_Sequences, [match_arity(false)]), Sequence_Part_Codes),
        maplist([Tuple, List]>>(Tuple =.. [row | List]), Csv_Sequences, Sequences).

middle(List, Value) :- 
        length(List, L),
        M is ceiling(L / 2),
        nth1(M, List, Value).

rule_check_lax(Sequence, [First, Second]) :- % First comes before Second semantics
        (
                nth1(First_Index, Sequence, First), nth1(Second_Index, Sequence, Second) ->
                        First_Index < Second_Index
                ; true
        ).

valid_sequences_lax(All_Sequences, Rules, Valid_Sequences) :-
        include([Sequence]>>maplist({Sequence}/[Rule]>>rule_check_lax(Sequence, Rule), Rules), All_Sequences, Valid_Sequences).

main :-
        process_input("input.txt", Rules, Sequences),
        valid_sequences_lax(Sequences, Rules, Valid_Sequences1),
        maplist(middle, Valid_Sequences1, Middle_Values1),
        sum_list(Middle_Values1, Solution1),
        write(Solution1).
