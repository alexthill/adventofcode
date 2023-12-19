:- ensure_loaded(utils).
:- set_prolog_flag(stack_limit, 4_294_967_296). % code needs a lot RAM

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(8901, 9064).

solve(EC, MaxEC) :-
    read_file_to_lines_codes('../inputs/day16.txt', Lines),
    empty_assoc(Map0), fold_grid(fold_cell, Lines, Map0, Map),
    find_energ(Map, 0'r-1-1, EC),
    start_positions(Lines, SPos), maplist(find_energ(Map), SPos, ECS), max_list(ECS, MaxEC).

fold_cell(Pos, V0, C, V) :- put_assoc(Pos, V0, C-[], V).

fold_count_energ(_-[], V, V).
fold_count_energ(_, V0, V) :- succ(V0, V).

start_positions(Grid, SPos) :-
    grid_size(Grid, W, H),
    numlist(1, W, Cols), numlist(1, H, Rows),
    maplist(dir_pos_pair_u(H), Cols, US), maplist(dir_pos_pair_d, Cols, DS),
    maplist(dir_pos_pair_l(W), Rows, LS), maplist(dir_pos_pair_r, Rows, RS),
    append([US, DS, LS, RS], SPos).

dir_pos_pair_d(Col, 0'd-Col-1).
dir_pos_pair_u(H, Col, 0'u-Col-H).
dir_pos_pair_r(Row, 0'r-1-Row).
dir_pos_pair_l(W, Row, 0'l-W-Row).

find_energ(Map, Dir-X-Y, EC) :- beam(Dir, X-Y, Map, Map1), !, assoc_to_values(Map1, Cells), foldl(fold_count_energ, Cells, 0, EC).

beam(Dir, Pos, M, M) :- get_assoc(Pos, M, _-DS), member(Dir, DS).
beam(Dir, Pos, M0, M) :- get_assoc(Pos, M0, C-DS, M1, C-[Dir|DS]), !, next_beam(C, Dir, Pos, M1, M).
beam(_, _, M, M).

next_beam(0'., 0'r, X-Y, M0, M) :- succ(X, X1), beam(0'r, X1-Y, M0, M).
next_beam(0'-, 0'r, X-Y, M0, M) :- succ(X, X1), beam(0'r, X1-Y, M0, M).
next_beam(0'|, 0'r, X-Y, M0, M) :- (succ(Y0, Y) -> beam(0'u, X-Y0, M0, M1) ; M1 = M0), (succ(Y, Y1) -> beam(0'd, X-Y1, M1, M) ; M = M1).
next_beam(0'/, 0'r, X-Y, M0, M) :- succ(Y0, Y), beam(0'u, X-Y0, M0, M).
next_beam(0'\\, 0'r, X-Y, M0, M) :- succ(Y, Y1), beam(0'd, X-Y1, M0, M).
next_beam(0'., 0'l, X-Y, M0, M) :- succ(X0, X), beam(0'l, X0-Y, M0, M).
next_beam(0'-, 0'l, X-Y, M0, M) :- succ(X0, X), beam(0'l, X0-Y, M0, M).
next_beam(0'|, 0'l, X-Y, M0, M) :- (succ(Y0, Y) -> beam(0'u, X-Y0, M0, M1) ; M1 = M0), (succ(Y, Y1) -> beam(0'd, X-Y1, M1, M) ; M = M1).
next_beam(0'/, 0'l, X-Y, M0, M) :- succ(Y, Y1), beam(0'd, X-Y1, M0, M).
next_beam(0'\\, 0'l, X-Y, M0, M) :- succ(Y0, Y), beam(0'u, X-Y0, M0, M).
next_beam(0'., 0'd, X-Y, M0, M) :- succ(Y, Y1), beam(0'd, X-Y1, M0, M).
next_beam(0'-, 0'd, X-Y, M0, M) :- (succ(X0, X) -> beam(0'l, X0-Y, M0, M1) ; M1 = M0), (succ(X, X1) -> beam(0'r, X1-Y, M1, M) ; M = M1).
next_beam(0'|, 0'd, X-Y, M0, M) :- succ(Y, Y1), beam(0'd, X-Y1, M0, M).
next_beam(0'/, 0'd, X-Y, M0, M) :- succ(X0, X), beam(0'l, X0-Y, M0, M).
next_beam(0'\\, 0'd, X-Y, M0, M) :- succ(X, X1), beam(0'r, X1-Y, M0, M).
next_beam(0'., 0'u, X-Y, M0, M) :- succ(Y0, Y), beam(0'u, X-Y0, M0, M).
next_beam(0'-, 0'u, X-Y, M0, M) :- (succ(X0, X) -> beam(0'l, X0-Y, M0, M1) ; M1 = M0), (succ(X, X1) -> beam(0'r, X1-Y, M1, M) ; M = M1).
next_beam(0'|, 0'u, X-Y, M0, M) :- succ(Y0, Y), beam(0'u, X-Y0, M0, M).
next_beam(0'/, 0'u, X-Y, M0, M) :- succ(X, X1), beam(0'r, X1-Y, M0, M).
next_beam(0'\\, 0'u, X-Y, M0, M) :- succ(X0, X), beam(0'l, X0-Y, M0, M).
