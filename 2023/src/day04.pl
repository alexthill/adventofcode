:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(20117, 13768818).

solve(Sum1, Sum2) :-
    read_file_to_lines_string('../inputs/day04.txt', Lines),
    maplist(win_number_count, Lines, Counts),
    maplist(points, Counts, Points),
    sum_list(Points, Sum1),
    same_length(CC0S, Lines),
    maplist(=(1), CC0S),
    card_counts(Counts, CC0S, CCS),
    sum_list(CCS, Sum2).

win_number_count(Line, Count) :- 
    split_string(Line, ":|", "", [_,WinNumsStr,NumsStr]),
    str_to_num_set(WinNumsStr, WinNums),
    str_to_num_set(NumsStr, Nums),
    ord_intersection(WinNums, Nums, NS),
    length(NS, Count).

str_to_num_set(Str, NumSet) :- string_codes(Str, Codes), all_nums(Codes, Nums), list_to_ord_set(Nums, NumSet).

points(0, 0).
points(Count, Points) :- Points is 2**(Count - 1).

card_counts([], [], []).
card_counts([C|CS], [CC0|CC0S], [CC0|CCS]) :- inc_card_count(C, CC0, CC0S, CC1S), card_counts(CS, CC1S, CCS).

inc_card_count(_, _, [], []).
inc_card_count(0, _, CCS, CCS).
inc_card_count(N, C, [CC0|CC0S], [CC|CCS]) :- CC is CC0 + C, succ(N0, N), inc_card_count(N0, C, CC0S, CCS).
