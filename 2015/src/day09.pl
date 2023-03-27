main :-
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(207, 804).

solve(P1, P2) :-
    open('../inputs/day09.txt', read, Str), read_file(Str, Dists, Locs0), close(Str),
    sort(Locs0, Locs),
    bagof(P, permutation(Locs, P), PS),
    maplist(get_dist(Dists), PS, DS), min_list(DS, P1), max_list(DS, P2).

read_file(Stream, [], []) :- at_end_of_stream(Stream).
read_file(Stream, [[A,B,D]|DS], [A,B|LS]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, String),
    split_string(String, " ", " =", [A0, _, B0, D0]),
    number_string(D, D0), sort([A0, B0], [A, B]),
    read_file(Stream, DS, LS).

get_dist(_, [_], 0).
get_dist(T, [A,B|LS], D) :- get_dist(T, [B|LS], D0), get_dist(T, A, B, D1), D is D0 + D1.
get_dist(T, A0, B0, D) :- sort([A0, B0], [A, B]), member([A, B, D], T).
