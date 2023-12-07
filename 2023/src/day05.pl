:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(389056265, 137516820).

solve(Min1, Min2) :-
    read_file_to_string('../inputs/day05.txt', Str, []),
    parse_input(Str, Seeds, Rules),
    maplist(foldl(map_value, Rules), Seeds, Dests),
    min_list(Dests, Min1),
    to_ranges(Seeds, Ranges),
    maplist(map_ranges_get_min(Rules), Ranges, DestRanges),
    maplist(min_list, DestRanges, DestMins),
    min_list(DestMins, Min2).

parse_input(Str, Seeds, Rules) :-
    split_string(Str, ":", "", [_|Parts]),
    maplist(string_codes, Parts, Codes),
    maplist(all_nums, Codes, [Seeds|Rules]).

map_value([], X, X).
map_value([D,S,L|_], X, Y) :- X >= S, X < S + L, Y is D + X - S.
map_value([_,_,_|Rules], X, Y) :- map_value(Rules, X, Y).

to_ranges([], []).
to_ranges([A,L|XS], [[A,AE]|RS]) :- AE is A + L, to_ranges(XS, RS).

map_ranges_get_min(Rules, R, Mins) :- foldl(map_ranges, Rules, [R], DRS), maplist(min_list, DRS, Mins).

map_ranges(_, [], []).
map_ranges(Rules, [R|RS], DRS) :- map_range(Rules, R, DRS0), map_ranges(Rules, RS, DRS1), append(DRS0, DRS1, DRS).

map_range([], X, [X]).
map_range([D,S,L|_], [A,AE], [[B,BE]]) :-
    A >= S, AE =< S + L,
    B is A - S + D, BE is AE - S + D.
map_range([D,S,L|Rules], [A,AE], [[B,BE]|RS]) :-
    A >= S, A < S + L,
    B is A - S + D, BE is D + L,
    C is S + L,
    map_range(Rules, [C,AE], RS).
map_range([D,S,L|Rules], [A,AE], [[D,BE]|RS]) :-
    AE > S, AE =< S + L,
    BE is AE - S + D,
    map_range(Rules, [A,S], RS).
map_range([D,S,L|Rules], [A,AE], [[D,BE]|RS]) :-
    A < S, AE > S + L,
    BE is D + L,
    C is S + L,
    map_range(Rules, [A,S], RS1), map_range(Rules, [C,AE], RS2), append(RS1, RS2, RS).
map_range([_,_,_|Rules], R, RS) :- map_range(Rules, R, RS).