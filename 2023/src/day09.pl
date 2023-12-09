:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(2043677056, 1062).

solve(NextSum, PrevSum) :-
    read_file_to_lines_codes('../inputs/day09.txt', Lines),
    maplist(all_nums, Lines, Nums),
    maplist(extrapolate, Nums, Prevs, Nexts),
    sum_list(Nexts, NextSum),
    sum_list(Prevs, PrevSum).

num_diffs([_], []).
num_diffs([A,B|XS], [D|DS]) :- D is B - A, num_diffs([B|XS], DS).

extrapolate(NS, 0, 0) :- maplist(=(0), NS).
extrapolate([N|NS], Prev, Next) :-
    num_diffs([N|NS], DS), extrapolate(DS, Prev0, Next0),
    last(NS, Last), Next is Last + Next0, Prev is N - Prev0.
