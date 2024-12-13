#!/usr/bin/env prolog

:- initialization main.

:- dynamic matrix/2.

build_table(T) :- build_table(T, 1).
build_table([], _Index).
build_table([THD|TTL], Current_Index) :- build_row(THD, Current_Index, 1), Next_Index is Current_Index + 1, build_table(TTL, Next_Index).
build_row([], _Row, _Column).
build_row([RHD|RTL], Row, Column) :- asserta(matrix(Row-Column, RHD)), Next_Column is Column + 1, build_row(RTL, Row, Next_Column).


direction(0-DColumn) :- member(DColumn, [1, -1]).
direction(DRow-0) :- member(DRow, [1, -1]).
forward_direction(0-1).
forward_direction(1-0).
adjacent(Row1-Column1, Row2-Column2, DRow-DColumn) :- plus(DRow, Row2, Row1), plus(DColumn, Column2, Column1).

regions(Regions) :-
        findall(Pos, matrix(Pos, _V), All_Positions),
        list_to_ord_set(All_Positions, All_Available_Set),
        regions(Regions, []-All_Available_Set).

regions(Regions_Before, Regions_Before-[]).
regions(Regions_Final, Regions_Before-[AHD|ATL]) :-
        region_of(AHD, Plot_Type-Fields),
        ord_subtract(ATL, Fields,  Available_After),
        Regions_After = [Plot_Type-Fields|Regions_Before], 
        regions(Regions_Final, Regions_After-Available_After).

region_of(Pos, Region) :- region_of(Region, _Plot_Type-[]-[Pos], [Pos]).
region_of(Region_Before, Region_Before-_Candidates_Checked, []).
region_of(Region, Plot_Type-Fields_Before-Candidates_Checked_Before, [Pos|Candidates_Rest]) :-
        (matrix(Pos, Plot_Type) -> % candidate is good: confirm as a field of the region; all previously undiscovered neighbors are also candidates
                ord_add_element(Fields_Before, Pos, Fields_After),
                findall(Neighbor, (
                                direction(Dir),
                                adjacent(Pos, Neighbor, Dir),
                                \+ ord_memberchk(Neighbor, Candidates_Checked_Before)
                        ), New_Neighbors_Raw),
                list_to_ord_set(New_Neighbors_Raw, New_Neighbors),
                ord_union(Candidates_Checked_Before, New_Neighbors, Candidates_Checked_After),
                append(New_Neighbors, Candidates_Rest, Candidates_After),
                region_of(Region, Plot_Type-Fields_After-Candidates_Checked_After, Candidates_After)
        ;
        true -> % candidate failed: just drop it
                region_of(Region, Plot_Type-Fields_Before-Candidates_Checked_Before, Candidates_Rest)
        ).

region_external_segments(Region_Fields, Segments) :-
        foldl([Pos, Segments_Before, Segments_After]>>(
                        findall(Dir, (
                                        direction(Dir),
                                        \+ (
                                                adjacent(Pos, Neighbor, Dir),
                                                ord_memberchk(Neighbor, Region_Fields)
                                        )
                                ), Open_Directions),
                        maplist({Pos}/[Dir, Pos-Dir]>>true, Open_Directions, Segments),
                        append(Segments, Segments_Before, Segments_After)
                ), Region_Fields, [], Segments).

region_sides(Segments, Sides) :-
        list_to_ord_set(Segments, Segments_Ordered),
        findall(Field1-Side_Dir, (
                        member(Field1-Side_Dir, Segments_Ordered),
                        forward_direction(Neighbor_Dir),
                        adjacent(Field1, Field2, Neighbor_Dir),
                        ord_memberchk(Field2-Side_Dir, Segments_Ordered)
                ), Redundant_Segments),
        length(Segments, Perimeter),
        length(Redundant_Segments, Redundant_From_Perimeter),
        Sides is Perimeter - Redundant_From_Perimeter.
        

region_cost_both(Region_Fields, Costs) :-
        length(Region_Fields, Area),
        region_external_segments(Region_Fields, Segments),
        length(Segments, Perimeter),
        region_sides(Segments, Sides),
        maplist([One_D_Value, Cost]>>(Cost is Area * One_D_Value), [Perimeter, Sides], Costs).

main :-
        csv_read_file("input.txt", Rows),
        maplist([row(Atom), Plot_Types]>>atom_chars(Atom, Plot_Types), Rows, Table),
        build_table(Table),
        regions(Regions),
        maplist([_Plot_Type-Fields, Cost1, Cost2]>>region_cost_both(Fields, [Cost1, Cost2]), Regions, Costs1, Costs2),
        maplist([Costs, Solution]>>sum_list(Costs, Solution), [Costs1, Costs2], Solutions),
        format("~d ~d", Solutions),
        halt.
