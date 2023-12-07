:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(54450, 54265).

solve(Sum, Sum2) :-
    read_file_to_lines_codes('../inputs/day01.txt', Lines),
    maplist(digits, Lines, Digits),
    maplist(first_and_last, Digits, Nums),
    maplist(digits2, Lines, Digits2),
    maplist(first_and_last, Digits2, Nums2),
    sum_list(Nums, Sum),
    sum_list(Nums2, Sum2).

digits([], []).
digits([X|XS], [Y|YS]) :- X >= 0'0, X =< 0'9, Y is X - 0'0, digits(XS, YS).
digits([_|XS], YS) :- digits(XS, YS).

digits2([], []).
digits2([0'o,0'n,0'e|XS], [1|YS]) :- digits2([0'e|XS], YS).
digits2([0't,0'w,0'o|XS], [2|YS]) :- digits2([0'o|XS], YS).
digits2([0't,0'h,0'r,0'e,0'e|XS], [3|YS]) :- digits2([0'e|XS], YS).
digits2([0'f,0'o,0'u,0'r|XS], [4|YS]) :- digits2(XS, YS).
digits2([0'f,0'i,0'v,0'e|XS], [5|YS]) :- digits2([0'e|XS], YS).
digits2([0's,0'i,0'x|XS], [6|YS]) :- digits2(XS, YS).
digits2([0's,0'e,0'v,0'e,0'n|XS], [7|YS]) :- digits2([0'n|XS], YS).
digits2([0'e,0'i,0'g,0'h,0't|XS], [8|YS]) :- digits2([0't|XS], YS).
digits2([0'n,0'i,0'n,0'e|XS], [9|YS]) :- digits2([0'e|XS], YS).
digits2([X|XS], [Y|YS]) :- X >= 0'0, X =< 0'9, Y is X - 0'0, digits2(XS, YS).
digits2([_|XS], YS) :- digits2(XS, YS).

first_and_last([], 0).
first_and_last([N], X) :- X is N *10 + N.
first_and_last([N|NS], X) :- last(NS, L), X is N * 10 + L.
