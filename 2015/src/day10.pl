main :-
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(360154, 5103798).

solve(P1, P2) :-
    read_file_to_codes('../inputs/day10.txt', Codes, []),
    maplist(plus(-48), Codes, Nums),
    repeat_look_and_say(40, Nums, Out1), length(Out1, P1),
    repeat_look_and_say(50, Nums, Out2), length(Out2, P2).
    

repeat_look_and_say(0, XS, XS).
repeat_look_and_say(N, XS, ZS) :- M is N - 1, look_and_say(XS, YS), repeat_look_and_say(M, YS, ZS).

look_and_say([], []).
look_and_say([X|XS], YS) :- look_and_say(X, 1, XS, YS).

look_and_say(L, C, [], [C,L]).
look_and_say(L, C0, [L|XS], YS) :- C is C0 + 1, look_and_say(L, C, XS, YS).
look_and_say(L, C, [X|XS], [C,L|YS]) :- look_and_say(X, 1, XS, YS).
