:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(2362, 6538).

solve(MaxL1, MaxL2) :-
    read_file_to_lines_codes('../inputs/day23.txt', Lines),
    grid_size(Lines, W, H), EndPos is W * H - 1,
    fold_grid(fold_cell, Lines, []-W, CellsList-_),
    list_to_assoc(CellsList, Cells),
    create_graph_and_find_path(1, W, EndPos, Cells, MaxL1),
    create_graph_and_find_path(2, W, EndPos, Cells, MaxL2).

fold_cell(_, V, 0'#, V).
fold_cell(X-Y, V-W, C, [Pos-C|V]-W) :- Pos is X + (Y - 1) * W.

create_graph_and_find_path(P, W, EndPos, Cells, MaxL) :-
    follow_path(P, W, 2, 0, 2-W, Cells-[], _-Edges),
    edges_nodes(Edges, Nodes),
    min_assoc(Nodes, S, SEL),
    (   P = 2, gen_assoc(BeforeEnd, Nodes, BeforeEndEL), member(EndPos-D, BeforeEndEL), writeln(BeforeEnd-EndPos-D)
    ->  find_longest_path(BeforeEnd, Nodes, [S-SEL], 0, MaxL0), MaxL is MaxL0 + D
    ;   find_longest_path(EndPos, Nodes, [S-SEL], 0, MaxL)).

find_longest_path(EndPos, Nodes, [N-EL|V], Len, MaxL) :- convlist(move_to_node(EndPos, Nodes, [N-EL|V], Len), EL, MaxLS), max_list(MaxLS, MaxL).

% 6391 + 147 is the correct answer for my input,
% the answer will be found long before the code finishes going through all pathes.
move_to_node(EndPos, _, _, Len0, EndPos-L, Len) :- Len is Len0 + L, (Len >= 6391 -> writeln(end-Len) ; true).
move_to_node(_, _, [_|V], _, Next-_, 0) :- member(Next-_, V).
move_to_node(EndPos, Nodes, [N-EL0|V], Len0, Next-L, MaxL) :-
    select(Next-L, EL0, EL), Len is Len0 + L,
    get_assoc(Next, Nodes, NextEL),
    find_longest_path(EndPos, Nodes, [Next-NextEL,N-EL|V], Len, MaxL).

follow_path(_, _, Node, Len, Pos-_, Cells-Edges, Cells-[Node-Pos-Len|Edges]) :- get_assoc(Pos, Cells, 0'x).
follow_path(P, W, Node, Len, Pos-Dir, Cells0-Edges0, Cells-Edges) :-
    get_dirs(W, Dirs0), DirOp is -Dir, include(\=(DirOp), Dirs0, Dirs),
    convlist(dir_to_next(P, Cells0-W, Pos), Dirs, Nexts),
    follow_all(P, Cells0, Cells, W, Node, Pos, Len, Edges0, Edges, Nexts).

follow_all(_, Cells, Cells, _, Node, Pos, Len, Edges, [Node-Pos-Len|Edges], []).
follow_all(P, Cells0, Cells, W, Node, _, Len0, Edges0, Edges, [Pos-Dir]) :-
    succ(Len0, Len),
    follow_path(P, W, Node, Len, Pos-Dir, Cells0-Edges0, Cells-Edges).
follow_all(1, Cells0, Cells, W, Node, Pos, Len, Edges0, Edges, Nexts) :-
    put_assoc(Pos, Cells0, 0'x, Cells1),
    foldl(follow_path(1, W, Pos, 1), Nexts, Cells1-[Node-Pos-Len|Edges0], Cells-Edges).
follow_all(2, Cells0, Cells, W, Node, Pos, Len, Edges0, Edges, Nexts) :-
    put_assoc(Pos, Cells0, 0'x, Cells1),
    foldl(follow_path(2, W, Pos, 1), Nexts, Cells1-[Node-Pos-Len,Pos-Node-Len|Edges0], Cells-Edges).

get_dirs(W, [1, -1, W, W2]) :- W2 is -W.

dir_to_next(2, Cells-_, Pos0, Dir, Pos-Dir) :-
    Pos is Pos0 + Dir, get_assoc(Pos, Cells, _).
dir_to_next(_, Cells-W, Pos0, Dir, Pos-Dir) :-
    Pos is Pos0 + Dir, get_assoc(Pos, Cells, C), slope_dir_ok(W, C, Dir).

slope_dir_ok(_, 0'>, 1).
slope_dir_ok(W, 0'v, W).
slope_dir_ok(_, 0'., _).
slope_dir_ok(_, 0'x, _).

edges_nodes([], NS) :- empty_assoc(NS).
edges_nodes([A-B-Len|ES], NS) :-
    edges_nodes(ES, NS0),
    (   get_assoc(A, NS0, EL0)
    ->  (member(B-Len2, EL0), Len2 > Len -> NS = NS0 ; put_assoc(A, NS0, [B-Len|EL0], NS))
    ;   put_assoc(A, NS0, [B-Len], NS)
    ).
