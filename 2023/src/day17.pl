:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(1238, 1362).

solve(Len, Len2) :-
    read_file_to_lines_codes('../inputs/day17.txt', Lines),
    find_shortest_path(Lines, 1-3, Len),
    find_shortest_path(Lines, 4-10, Len2).

find_shortest_path(Lines, Min-Max, Len) :-
    fold_grid(fold_cell(Max), Lines, [], MapList),
    list_to_assoc([1-1-r-0-(0-0),1-1-d-0-(0-0)|MapList], Map0),
    grid_size(Lines, W, H),
    list_to_heap([0-(1-1-r-0), 0-(1-1-d-0)], Heap0),
    process_next_min(W-H, Min-Max, Map0, Heap0, Map, _),
    findall(X, (gen_assoc(W-H-_-N, Map, _-X), N >= Min), XS), min_list(XS, Len).

fold_cell(Max, Pos, V0, C, V) :- numlist(1, Max, Nums), cartesian([u,r,d,l], Nums, CP), foldl(add_cell_entry(Pos, C), CP, V0, V).

add_cell_entry(Pos, C, Dir-N, V0, [Pos-Dir-N-Val|V0]) :- C1 is C - 0'0, Val = C1-999999999.

cartesian(AS, BS, CS) :- findall(C, (member(A, AS), member(B, BS), C = A-B), CS).

process_next_min(Size, Min-_, M, H0, M, H) :- get_from_heap(H0, _, Size-_-N, H), N >= Min.
process_next_min(Size, LS, M0, H0, M, H) :-
    get_from_heap(H0, Dist, Key, H1), del_assoc(Key, M0, _, M1),
    nexts(LS, Key, Nexts), foldl(insert_heap(Dist), Nexts, M1-H1, M2-H2),
    !, process_next_min(Size, LS, M2, H2, M, H).
process_next_min(_, _, M, H, M, H).

insert_heap(Dist, Key, M0-H0, M-H) :-
    get_assoc(Key, M0, C-Dist0), Dist1 is Dist + C, Dist1 < Dist0, put_assoc(Key, M0, C-Dist1, M), add_to_heap(H0, Dist1, Key, H).
insert_heap(_, _, M-H, M-H).

nexts(LS, Pos-Dir-N, Nexts) :- next_dirs(LS, Dir, N, Dirs), maplist(calc_next(Pos, Dir, N), Dirs, Nexts).

calc_next(Pos, Dir, N0, Dir, X-Y-Dir-N) :- succ(N0, N), next_pos(Dir, Pos, X-Y).
calc_next(Pos, _, _, Dir, X-Y-Dir-1) :- next_pos(Dir, Pos, X-Y).

next_dirs(Min-_, D, N, [D]) :- N < Min.
next_dirs(_-Max, Dir, Max, [A,B]) :- perpendiular(Dir, A, B).
next_dirs(_, Dir, _, [Dir,A,B]) :- perpendiular(Dir, A, B).

perpendiular(r, u, d).
perpendiular(l, u, d).
perpendiular(u, l, r).
perpendiular(d, l, r).

next_pos(u, X-Y0, X-Y) :- Y is Y0 - 1.
next_pos(d, X-Y0, X-Y) :- Y is Y0 + 1.
next_pos(l, X0-Y, X-Y) :- X is X0 - 1.
next_pos(r, X0-Y, X-Y) :- X is X0 + 1.
