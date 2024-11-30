:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(6828, _).

solve(PathLen, InCount) :-
    read_file_to_lines_codes('../inputs/day10.txt', Lines),
    fold_grid(iter_cell, Lines, [], Cells),
    list_to_assoc(Cells, GA),
    map_s(GA, Cells, [S,A,B]),
    find_path(GA, S, A, S, B, PathLen, Path),
    list_to_ord_set([S|Path], PathOrd),
    include(cell_is_in(PathOrd, GA), Cells, In),
    %writeln(In),
    length(In, InCount).

fold_grid(Fn, [R|RS], V0, V) :- length(R, W), length([R|RS], H), fold_grid(Fn, [R|RS], W, H, W, H, V0, V).
fold_grid(_, [], _, _, _, _, V0, V0).
fold_grid(Fn, [[]|CS], W, H, _, Y, V0, V) :- succ(Y0, Y), fold_grid(Fn, CS, W, H, W, Y0, V0, V).
fold_grid(Fn, [[C|CS]|CSS], W, H, X, Y, V0, V) :- succ(X0, X), call(Fn, C, [Y,X], V1, V), fold_grid(Fn, [CS|CSS], W, H, X0, Y, V0, V1).

iter_cell(C, Pos, V, [Pos-Adjs|V]) :- cell_adjs(C, Pos, Adjs).
cell_adjs(0'|, [Y,X], [[Y0,X],[Y1,X]]) :- succ(Y0, Y), succ(Y, Y1).
cell_adjs(0'-, [Y,X], [[Y,X0],[Y,X1]]) :- succ(X0, X), succ(X, X1).
cell_adjs(0'L, [Y,X], [[Y1,X],[Y,X0]]) :- succ(X0, X), succ(Y, Y1).
cell_adjs(0'J, [Y,X], [[Y1,X],[Y,X1]]) :- succ(X, X1), succ(Y, Y1).
cell_adjs(0'F, [Y,X], [[Y0,X],[Y,X0]]) :- succ(X0, X), succ(Y0, Y).
cell_adjs(0'7, [Y,X], [[Y0,X],[Y,X1]]) :- succ(X, X1), succ(Y0, Y).
cell_adjs(0'., _, 0'.).
cell_adjs(0'S, _, 0'S).

map_s(_, [], []).
map_s(GA, [[Y,X]-0'S|_], [[Y,X],A,B]) :-
    cell_adjs(_, [Y,X], [A,B]),
    (get_assoc(A, GA, [[Y,X],_]); get_assoc(A, GA, [_,[Y,X]])),
    (get_assoc(B, GA, [[Y,X],_]); get_assoc(B, GA, [_,[Y,X]])),
    dif(A,B).
map_s(GA, [_|XS], S) :- map_s(GA, XS, S).

find_path(_, _, A, _, A, 1, [A]).
find_path(_, A0, A, A, A0, 0, []).
find_path(GA, A0, A, B0, B, N, [A,B|PS]) :-
    (get_assoc(A, GA, [A0,A1]); get_assoc(A, GA, [A1,A0])),
    (get_assoc(B, GA, [B0,B1]); get_assoc(B, GA, [B1,B0])),
    find_path(GA, A, A1, B, B1, N0, PS), succ(N0, N).

count_intersections(_, _, [_,0], 0).
count_intersections(P, GA, [Y,X], N) :-
    ord_memberchk([Y,X], P),
    succ(X0, X), count_intersections(P, GA, [Y,X0], N0),
    cell_inter_val(GA, [Y,X], V), N is N0 + V.
count_intersections(P, GA, [Y,X], N) :- succ(X0, X), count_intersections(P, GA, [Y,X0], N).

cell_inter_val(GA, C, N) :- get_assoc(C, GA, Adjs), cell_adjs(T, C, Adjs), cell_inter_val(T, N).
cell_inter_val(0'|, 1).
cell_inter_val(0'L, 1).
cell_inter_val(0'J, 1).
cell_inter_val(_, 0).

cell_is_in(P, GA, C-_) :- \+ ord_memberchk(C, P), count_intersections(P, GA, C, N), !, 1 =:= N mod 2.

% 460 high