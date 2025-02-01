:- ensure_loaded(utils).

main :-
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(7402, 3384337640277).

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
    empty_assoc(C),
    arrange(C, _, P1, Nums, N).

map_line2(Line, N) :-
    split_string(Line, " ", "", LineParts),
    maplist(string_codes, LineParts, [P1,P2]),
    all_nums(P2, Nums),
    append([P1,`?`,P1,`?`,P1,`?`,P1,`?`,P1], P1Repeat),
    append([Nums,Nums,Nums,Nums,Nums], NumsRepeat),
    empty_assoc(C),
    arrange(C, _, P1Repeat, NumsRepeat, N).

arrange(C, C, [], [], 1).
arrange(C, C, XS, RS, N) :- get_assoc(XS-RS, C, N).
arrange(CI, CO, [0'.|XS], RS, N) :- arrange(CI, CO, XS, RS, N).
arrange(CI, CO, [0'#|XS], [R|RS], N) :- succ(R0, R), arrange2(CI, CO, XS, R0, RS, N).
arrange(CI, CO1, [0'?|XS], RS, N) :-
    (arrange(CI, CI1, XS, RS, N1); N1 is 0, CI1 = CI), !, % these cuts improve performence for some reason
    (arrange(CI1, CO, [0'#|XS], RS, N2); N2 is 0, CO = CI1), !,
    N is N1 + N2,
    put_assoc([0'?|XS]-RS, CO, N, CO1).

arrange2(C, C, [], 0, [], 1).
arrange2(CI, CO, [0'.|XS], 0, RS, N) :- arrange(CI, CO, XS, RS, N).
arrange2(CI, CO, [0'?|XS], 0, RS, N) :- arrange(CI, CO, XS, RS, N).
arrange2(CI, CO, [0'#|XS], R, RS, N) :- succ(R0, R), arrange2(CI, CO, XS, R0, RS, N).
arrange2(CI, CO, [0'?|XS], R, RS, N) :- succ(R0, R), arrange2(CI, CO, XS, R0, RS, N).
