#!/usr/bin/env prolog

:- initialization main.

calculate_mul(Match, Product) :-
        maplist(number_string, Factors, [Match.get(1), Match.get(2)]),
        foldl([Current, Acc, New]>>(New is Acc*Current), Factors, 1, Product).

extract_type(String, Type) :-
        re_matchsub("^(.*?)\\(", String, Match),
        Type = Match.get(1).

process_match(Match, Acc, New) :-
        Full_Text = Match.get(0),
        extract_type(Full_Text, Type),
        process_match(Type, Match, Acc, New).

process_match("mul", _Match, {mode: "don't", sum: Acc_Sum}, {mode: "don't", sum: Acc_Sum}).
process_match("mul", Match, {mode: "do", sum: Acc_Sum}, {mode: "do", sum: New_Sum}) :-
        calculate_mul(Match, Product), New_Sum is Acc_Sum + Product.

process_match("do", _Match, {mode: _M, sum: Acc_Sum}, {mode: "do", sum: Acc_Sum}).
process_match("don't", _Match, {mode: _M, sum: Acc_Sum}, {mode: "don't", sum: Acc_Sum}).


main :-
        read_file_to_string("input.txt", Content, []),
        Patterns = ["mul\\((\\d{1,3}),(\\d{1,3})\\)", "mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\)"],
        maplist(
                [Pattern, Sum]>>re_foldl(process_match, Pattern, Content, {mode: "do", sum: 0}, {mode: _M, sum: Sum}, []),
                Patterns,
                Sums
                ),
        format("~d ~d", Sums).
