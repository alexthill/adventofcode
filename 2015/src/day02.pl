:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(1606483, 3842356).

solve(Paper, Ribon) :-
    open('../inputs/day02.txt', read, Str),
    read_file_to_nums(Str, Lines),
    close(Str),
    maplist(msort, Lines, LinesSorted),
    paper_and_ribon(LinesSorted, 0, 0, Paper, Ribon).

paper_and_ribon([], P, R, P, R).
paper_and_ribon([[L, W, H]|XS], P0, R0, P, R) :-
    P1 is P0 + 2*L*W + 2*W*H + 2*H*L + L*W,
    R1 is R0 + 2*L + 2*W + L*W*H,
    paper_and_ribon(XS, P1, R1, P, R).
