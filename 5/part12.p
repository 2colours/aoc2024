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

rule_check(Sequence, [First, Second]) :- % First comes before Second semantics
        nth1(First_Index, Sequence, First),
        nth1(Second_Index, Sequence, Second),
        First_Index < Second_Index.

relevant_rule(Sequence, [First, Second]) :-
        member(First, Sequence),
        member(Second, Sequence).

relevant_rules(Sequence, Rules, Relevant) :-
        include([Rule]>>relevant_rule(Sequence, Rule), Rules, Relevant).

rule_edge([First, Second], First-Second).

fixed_sequence_order(Sequence, Rules, Fixed_Sequence) :-
        relevant_rules(Sequence, Rules, Relevant_Rules),
        maplist(rule_edge, Relevant_Rules, Edges),
        vertices_edges_to_ugraph(Sequence, Edges, Graph),
        top_sort(Graph, Fixed_Sequence).

main :-
        process_input("input.txt", Rules, Sequences),
        partition({Rules}/[Sequence]>>(
                        relevant_rules(Sequence, Rules, Relevant_Rules),
                        maplist({Sequence}/[Rule]>>rule_check(Sequence, Rule), Relevant_Rules)
                ), Sequences, Valid_Sequences, Invalid_Sequences),
        maplist({Rules}/[Sequence, Fixed_Sequence]>>fixed_sequence_order(Sequence, Rules, Fixed_Sequence), Invalid_Sequences, Fixed_Sequences),
        maplist([Sequence, Solution]>>(
                maplist(middle, Sequence, Middle_Values),
                sum_list(Middle_Values, Solution)
        ), [Valid_Sequences, Fixed_Sequences], Solutions),
        format("~d ~d", Solutions).
