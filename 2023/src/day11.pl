:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(9536038, 447744640566).

solve(DistSum, DistSum2) :-
    read_file_to_lines_codes('../inputs/day11.txt', [L|LS]),
    length(L, W), length([L|LS], H),
    fold_grid(fold_cell_occ, [L|LS], [[],[],[]], [PS,OYS,OXS]),
    list_to_ord_set(OXS, OX), list_between(1, W, AX), ord_subtract(AX, OX, UX),
    list_to_ord_set(OYS, OY), list_between(1, H, AY), ord_subtract(AY, OY, UY),
    maplist(expand_space(2, UX, UY), PS, PS1),
    setof(A, both_in(PS1, A), Pairs),
    foldl(fold_pair, Pairs, 0, DistSum),
    maplist(expand_space(1000000, UX, UY), PS, PS2),
    setof(A, both_in(PS2, A), Pairs2),
    foldl(fold_pair, Pairs2, 0, DistSum2).

fold_grid(Fn, [R|RS], V0, V) :- length(R, W), length([R|RS], H), fold_grid(Fn, [R|RS], W, H, W, H, V0, V).
fold_grid(_, [], _, _, _, _, V0, V0).
fold_grid(Fn, [[]|CS], W, H, _, Y, V0, V) :- succ(Y0, Y), fold_grid(Fn, CS, W, H, W, Y0, V0, V).
fold_grid(Fn, [[C|CS]|CSS], W, H, X, Y, V0, V) :- succ(X0, X), call(Fn, C, [Y,X], V1, V), fold_grid(Fn, [CS|CSS], W, H, X0, Y, V0, V1).

fold_cell_occ(0'., _, V, V).
fold_cell_occ(0'#, [Y,X], [PS,YS,XS], [[[Y,X]|PS],[Y|YS],[X|XS]]).

list_between(S, S, [S]).
list_between(S, E, [S|XS]) :- succ(S, S1), list_between(S1, E, XS).

expand_space(ExFact, UX, UY, [Y0,X0], [Y,X]) :- expand_dir(ExFact, UX, X0, X), expand_dir(ExFact, UY, Y0, Y).

expand_dir(ExFact, US, X0, X) :- include(>(X0), US, US1), length(US1, L), X is X0 + (ExFact - 1) * L.

both_in(L, Out) :- member(A, L), member(B, L), dif(A, B), sort([A,B], Out).

fold_pair([[AY,AX],[BY,BX]], D0, D) :- D is abs(AY - BY) + abs(AX - BX) + D0.
