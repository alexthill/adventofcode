:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(840336, 41382569).

solve(Prod, P2) :-
    read_file_to_lines_codes('../inputs/day06.txt', Lines),
    maplist(all_nums, Lines, [TS, DS]),
    maplist(valid_times, TS, DS, VS),
    maplist(length, VS, Counts),
    foldl(product, Counts, 1, Prod),
    maplist(include(dif(32)), Lines, Lines2),
    maplist(all_nums, Lines2, [[T], [D]]),
    solve_eq(-1, T, -D, [X1,X2]),
    P2 is floor(X1) - ceil(X2) + 1.

product(A, B, P) :- P is A * B.

valid_times(T, D, VS) :- check_times(T, D, T, VS).

check_times(_, _, 0, []).
check_times(T, D, N, [N|VS]) :- N * (T - N) > D, succ(N0, N), check_times(T, D, N0, VS).
check_times(T, D, N, VS) :- succ(N0, N), check_times(T, D, N0, VS).

solve_eq(A, B, C, []) :- B**2 - 4*A*C < 0.
solve_eq(A, B, C, [X1,X2]) :-
    D is B**2 - 4*A*C,
    S is sqrt(D),
    X1 is (-B-S)/(2*A),
    X2 is (-B+S)/(2*A).