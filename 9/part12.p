#!/usr/bin/env prolog

:- initialization main.

accumulate_content(Amount, Filled_Before-Empty_Before-Cursor_Before-filled-Next_ID, [Cursor_Before-(Next_ID-Amount)|Filled_Before]-Empty_Before-Cursor_After-empty-New_ID) :-
        Cursor_After is Cursor_Before + Amount,
        New_ID is Next_ID + 1.

accumulate_content(Amount, Filled_Before-Empty_Before-Cursor_Before-empty-Next_ID, Filled_Before-[Cursor_Before-Amount|Empty_Before]-Cursor_After-filled-Next_ID) :-
        Cursor_After is Cursor_Before + Amount.

read_input(File_Name, Filled_Blocks, Empty_Blocks) :-
        read_file_to_string(File_Name, Content, []),
        string_chars(Content, Content_Chars),
        include(\=('\n'), Content_Chars, Content_Chars_Trimmed),
        foldl([Char, Before, After]>>(number_chars(N, [Char]), accumulate_content(N, Before, After)), Content_Chars_Trimmed, []-[]-0-filled-0, Filled_Blocks-Empty_Blocks-_Cursor-_Next_Type-_Next_ID).
        

move_filled_block(part1, Filled_Block_To_Move, Filled_Blocks_Before-[], [Filled_Block_To_Move|Filled_Blocks_Before]-[]).
move_filled_block(part1, FStart-(FValue-FLength), Filled_Blocks_Before-[EStart-ELength|ERest], Filled_Blocks_After-Empty_Blocks_After) :-
        (FStart < EStart ->
                Empty_Blocks_After = [EStart-ELength|ERest],
                Filled_Blocks_After = [FStart-(FValue-FLength)|Filled_Blocks_Before]
        ;
        FLength < ELength -> % fits into current slot and there is leftover from the slot
                EStart_After is EStart + FLength,
                ELength_After is ELength - FLength,
                Empty_Blocks_After = [EStart_After-ELength_After|ERest],
                Filled_Blocks_After = [EStart-(FValue-FLength)|Filled_Blocks_Before]
        ;
        FLength = ELength -> % fits into current slot, no leftover
                Empty_Blocks_After = ERest,
                Filled_Blocks_After = [EStart-(FValue-FLength)|Filled_Blocks_Before]
        ;
        true -> % does not fit into current slot at all - fill it and carry on with the other slots and the leftover to fill
                FStart_After is FStart + ELength,
                FLength_After is FLength - ELength,
                move_filled_block(part1, FStart_After-(FValue-FLength_After), [EStart-(FValue-ELength)|Filled_Blocks_Before]-ERest, Filled_Blocks_After-Empty_Blocks_After)
        ).

move_filled_block(part2, FStart-(FValue-FLength), Filled_Blocks_Before-Empty_Blocks_Before, [Filled_Block_Moved|Filled_Blocks_Before]-Empty_Blocks_After) :-
        (nth0(Index, Empty_Blocks_Before, EStart-ELength, Empty_Blocks_Rest), FLength =< ELength, EStart < FStart ->
                Filled_Block_Moved = EStart-(FValue-FLength),
                (FLength = ELength -> % perfect fit: the empty block simply disappears
                        Empty_Blocks_After = Empty_Blocks_Rest
                ;
                true -> % the block remains but slides to the right and shortens just as much
                        ELength_After is ELength - FLength,
                        EStart_After is EStart + FLength,
                        nth0(Index, Empty_Blocks_After, EStart_After-ELength_After, Empty_Blocks_Rest)
                )
        ;
        true ->
                Empty_Blocks_After = Empty_Blocks_Before,
                Filled_Block_Moved = FStart-(FValue-FLength)
        ).

compress_blocks(Part, Filled_Blocks_To_Move, Empty_Blocks, Filled_Blocks_Moved) :- %Filled_Blocks_To_Move: descending order, Empty_Blocks: ascending order!
        foldl(move_filled_block(Part), Filled_Blocks_To_Move, []-Empty_Blocks, Filled_Blocks_Moved-_Empty_Blocks_End).

add_product_filled_block(FStart-(FValue-FLength), Sum_Before, Sum_After) :-
        Sum_Current is div((2 * FStart + FLength - 1) * FLength, 2) * FValue,
        Sum_After is Sum_Before + Sum_Current.

calculate_checksum(Filled_Blocks, Checksum) :-
        foldl(add_product_filled_block, Filled_Blocks, 0, Checksum).

main :-
        read_input("input.txt", Filled_Blocks, Empty_Blocks), % make sure trailing empty blocks are dropped!
        sort(0, @>, Filled_Blocks, Filled_Blocks_Desc),
        sort(0, @<, Empty_Blocks, Empty_Blocks_Asc),
        maplist([Part, Solution]>>(
                compress_blocks(Part, Filled_Blocks_Desc, Empty_Blocks_Asc, Compressed_Filled_Blocks),
                calculate_checksum(Compressed_Filled_Blocks, Solution)
                ),
                [part1, part2],
                Solutions),
        format("~d ~d", Solutions),
        halt.
