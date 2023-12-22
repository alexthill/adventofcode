:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(389, 70609).

solve(RC, FCSum) :-
    read_file_to_lines_codes('../inputs/day22.txt', Lines),
    maplist(parse_line, Lines, Bricks0),
    keysort(Bricks0, Bricks1),
    pairs_values(Bricks1, Bricks2),
    maplist(get_dict(z2), Bricks2, Z2S), max_list(Z2S, Zmax),
    findall(X-Y, (between(1, Zmax, X), Y = []), ZListPairs),
    list_to_assoc(ZListPairs, BricksByZTop0),
    foldl(fold_bricks_ztop, Bricks2, BricksByZTop0, BricksByZTop1),
    foldl(fall, Bricks2, BricksByZTop1, BricksByZTop),
    assoc_to_list(BricksByZTop, BBZTL), pairs_values(BBZTL, BBZTLV), append(BBZTLV, Bricks),
    list_to_assoc(ZListPairs, BricksByZBot0),
    foldl(fold_bricks_zbot, Bricks, BricksByZBot0, BricksByZBot),
    partition(can_remove(BricksByZBot), Bricks, Rem, NonRem),
    length(Rem, RC), maplist(fall_count(Bricks, BricksByZTop), NonRem, FCS), sum_list(FCS, FCSum).

parse_line(Line, Z1-brick{x1:X1, x2:X2, y1:Y1, y2:Y2, z1:Z1, z2:Z2, sup_by:[]}) :- all_nums(Line, [X1,Y1,Z1,X2,Y2,Z2]).

fold_bricks_zbot(B, BricksByZBot0, BricksByZBot) :- get_assoc(B.z1, BricksByZBot0, BS, BricksByZBot, [B|BS]).
fold_bricks_ztop(B, BricksByZTop0, BricksByZTop) :- get_assoc(B.z2, BricksByZTop0, BS, BricksByZTop, [B|BS]).

fall(B, BricksByZTop, BricksByZTop) :- B.z1 = 1.
fall(B0, BricksByZTop0, BricksByZTop) :-
    brick_supports(BricksByZTop0, B0, Sups), Sups \= [],
    B = B0.put(sup_by, Sups), get_assoc(B.z2, BricksByZTop0, BS0, BricksByZTop, BS), select(B0, BS0, B, BS).
fall(B, BricksByZTop0, BricksByZTop) :-
    Z1 is B.z1 - 1, Z2 is B.z2 - 1, BDown = B.put(_{z1:Z1, z2:Z2}),
    get_assoc(B.z2, BricksByZTop0, BS0, BricksByZTop1, BS), select(B, BS0, BS),
    get_assoc(Z2, BricksByZTop1, AS0, BricksByZTop2, AS), select(BDown, AS, AS0),
    fall(BDown, BricksByZTop2, BricksByZTop).
    
fall_one_down(B, X, X) :- B.z1 = 1.
fall_one_down(B, N-BricksByZTop, N-BricksByZTop) :- brick_supports(BricksByZTop, B, Sups), Sups \= [].
fall_one_down(B, N0-BricksByZTop0, N-BricksByZTop) :-
    get_assoc(B.z2, BricksByZTop0, BS0, BricksByZTop, BS),
    select(B, BS0, BS), succ(N0, N).

brick_supports(BricksByZTop, B, Sups) :-
    Z1 is B.z1 - 1, get_assoc(Z1, BricksByZTop, BSUnder),
    include(intersects(B), BSUnder, Sups).

brick_not_only_supported_by(Sup, B) :- B.sup_by \= [Sup].

intersects(A, B) :- A.x2 >= B.x1, B.x2 >= A.x1, A.y2 >= B.y1, B.y2 >= A.y1.

can_remove(BricksByZBot, B) :-
    Z2 is B.z2 + 1, get_assoc(Z2, BricksByZBot, BSOver),
    maplist(brick_not_only_supported_by(B), BSOver).

fall_count(Bricks0, BricksByZTop0, B, N) :-
    get_assoc(B.z2, BricksByZTop0, BS0, BricksByZTop, BS),
    select(B, BS0, BS), select(B, Bricks0, Bricks),
    foldl(fall_one_down, Bricks, 0-BricksByZTop, N-_).
