:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(6828, 459).

solve(MaxDist, InCount) :-
    read_file_to_lines_codes('../inputs/day10.txt', Lines),
    fold_grid(fold_cell, Lines, [], Cells),
    list_to_assoc(Cells, Grid),
    find_start_adj(Grid, Cells, S-A-_),
    find_path(Grid, S, S, A, Path),
    length(Path, PathLen), MaxDist is PathLen // 2,
    sort(Path, PathOrd),
    count_ins(Lines, 1, PathOrd, InCounts),
    sum_list(InCounts, InCount).

% Part 2 %

count_ins([], _, _, []).
count_ins([L|Ls], Y, Path, [N|Ns]) :-
    count_in(L, 1-Y, Path, 0, N),
    succ(Y, Y1),
    count_ins(Ls, Y1, Path, Ns).

count_in([], _, _, _, 0).
count_in([C|Cs], X-Y, Path, I, N) :-
    ord_memberchk(X-Y, Path),
    update_intersection_count(C, I, I1),
    succ(X, X1),
    count_in(Cs, X1-Y, Path, I1, N).
count_in([_|Cs], X-Y, Path, I, N) :-
    0 =:= I mod 2,
    succ(X, X1),
    count_in(Cs, X1-Y, Path, I, N).
count_in([_|Cs], X-Y, Path, I, N) :-
    succ(X, X1),
    count_in(Cs, X1-Y, Path, I, N0),
    succ(N0, N).

update_intersection_count(0'|, N, N1) :- succ(N, N1).
update_intersection_count(0'F, N, N1) :- succ(N, N1).
update_intersection_count(0'7, N, N1) :- succ(N, N1).
% uncomment the next line if S is in a F or 7 position and not L or J
% update_intersection_count(0'S, N, N1) :- succ(N, N1).
update_intersection_count(_, N, N).


% Part 1 %

find_path(_, S, A, S, [A]).
find_path(Grid, S, A0, A, [A0|Path]) :-
    (get_assoc(A, Grid, [A0,A1]); get_assoc(A, Grid, [A1,A0])),
    find_path(Grid, S, A, A1, Path).

find_start_adj(_, [], _) :- writeln('No Start found').
find_start_adj(Grid, [S-0'S|_], S-A-B) :-
    cell_adj(_, S, [A,B]),
    (get_assoc(A, Grid, [S,_]); get_assoc(A, Grid, [_,S])),
    (get_assoc(B, Grid, [S,_]); get_assoc(B, Grid, [_,S])),
    dif(A, B).
find_start_adj(Grid, [_|Cells], S) :- find_start_adj(Grid, Cells, S).

fold_cell(Pos, V, C, [Pos-Adjs|V]) :- cell_adj(C, Pos, Adjs).
fold_cell(_, V, _, V).

cell_adj(0'|, X-Y, [X-Y0,X-Y1]) :- succ(Y0, Y), succ(Y, Y1).
cell_adj(0'-, X-Y, [X0-Y,X1-Y]) :- succ(X0, X), succ(X, X1).
cell_adj(0'L, X-Y, [X-Y0,X1-Y]) :- succ(X, X1), succ(Y0, Y).
cell_adj(0'J, X-Y, [X0-Y,X-Y0]) :- succ(X0, X), succ(Y0, Y).
cell_adj(0'F, X-Y, [X1-Y,X-Y1]) :- succ(X, X1), succ(Y, Y1).
cell_adj(0'7, X-Y, [X0-Y,X-Y1]) :- succ(X0, X), succ(Y, Y1).
cell_adj(0'S, _, 0'S).
