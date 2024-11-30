:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(21, 525152).

solve(Sum, Sum2) :-
    read_file_to_lines_string('../inputs/day12.txt', Lines),
    maplist(map_line, Lines, Counts),
    sum_list(Counts, Sum),
    maplist(map_line2, Lines, Counts2),
    sum_list(Counts2, Sum2).

map_line(Line, N) :-
    split_string(Line, " ", "", LineParts),
    maplist(string_codes, LineParts, [P1,P2]),
    all_nums(P2, Nums),
    blubb(P1, Nums, N).

map_line2(Line, N) :-
    split_string(Line, " ", "", LineParts),
    maplist(string_codes, LineParts, [P1,P2]),
    all_nums(P2, Nums),
    append([P1,`?`,P1,`?`,P1,`?`,P1,`?`,P1], P1Repeat),
    append([Nums,Nums,Nums,Nums,Nums], NumsRepeat),
    blubb(P1Repeat, NumsRepeat, N), writeln(N).

arrange([], [], 1).
arrange([0'#|XS], [R|RS], N) :- succ(R0, R), arrange(XS, R0, RS, N).
arrange([0'?|XS], RS, N) :- arrange(XS, RS, N1), arrange([0'#|XS], RS, N2), !, N is N1 + N2.
arrange(_, _, 0).

arrange([], 0, [], 1).
arrange([0'?|XS], 0, RS, N) :- arrange(XS, RS, N).
arrange([0'#|XS], R, RS, N) :- succ(R0, R), arrange(XS, R0, RS, N).
arrange([0'?|XS], R, RS, N) :- succ(R0, R), arrange(XS, R0, RS, N).
arrange(_, _, _, 0).

blubb(XS, RS, N) :- string_codes(Str, XS), split_string(Str, ".", ".", Strs), maplist(string_codes, Strs, YS), blubber(YS, RS, N).

blubber([], [], 1).
blubber([], _, 0).
blubber(_, [], 0).
blubber(XS, RS, N) :- bagof([A,B], append(A, B, RS), ABS), maplist(blubbert(XS), ABS, NS), sum_list(NS, N).

blubbert([X|_], [[],_], 0) :- member(0'#, X).
blubbert([X|XS], [[1],R2], N) :- maplist(=(0'?), X), length(X, N1), blubber(XS, R2, N2), !, N is N1 * N2.
blubbert([X|XS], [R1,R2], N) :- arrange(X, R1, N1), (N1 =:= 0 -> N is 0 ; blubber(XS, R2, N2), !, N is N1 * N2).

non_empty_list_append([A|AS], BS, CS) :- append([A|AS], BS, CS).