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
    arrange(P1, Nums, N).

map_line2(Line, N) :-
    split_string(Line, " ", "", LineParts),
    maplist(string_codes, LineParts, [P1,P2]),
    all_nums(P2, Nums),
    append([P1,`?`,P1,`?`,P1,`?`,P1,`?`,P1], P1Repeat),
    append([Nums,Nums,Nums,Nums,Nums], NumsRepeat),
    arrange(P1Repeat, NumsRepeat, N), writeln(N).

arrange([], [], 1).
arrange([0'.|XS], RS, N) :- arrange(XS, RS, N).
arrange([0'#|XS], [R|RS], N) :- succ(R0, R), arrange(XS, R0, RS, N).
arrange([0'?|XS], RS, N) :- arrange(XS, RS, N1), arrange([0'#|XS], RS, N2), !, N is N1 + N2.
arrange(_, _, 0).

arrange([], 0, [], 1).
arrange([0'.|XS], 0, RS, N) :- arrange(XS, RS, N).
arrange([0'?|XS], 0, RS, N) :- arrange(XS, RS, N).
arrange([0'#|XS], R, RS, N) :- succ(R0, R), arrange(XS, R0, RS, N).
arrange([0'?|XS], R, RS, N) :- succ(R0, R), arrange(XS, R0, RS, N).
arrange(_, _, _, 0).
