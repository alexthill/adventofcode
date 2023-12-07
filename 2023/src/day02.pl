:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(2617, 59795).

solve(IdSum, PowerSum) :-
    read_file_to_lines_string('../inputs/day02.txt', Lines),
    maplist(get_cubes, Lines, Cubes),
    maplist(max_cubes, Cubes, MaxCubes),
    foldl(find_games, MaxCubes, [1, 0], [_, IdSum]),
    foldl(find_powers, MaxCubes, 0, PowerSum).

get_cubes(Line, Cubes) :- split_string(Line, ":;", "", [_|CubesRaw]), maplist(parse_selection, CubesRaw, Cubes).

parse_selection(String, Cubes) :- split_string(String, ",", " ", Substrings), order_selection(Substrings, Cubes).

order_selection([], [0, 0, 0]).
order_selection([X|XS], [R, G, Num]) :- sub_string(X, Before, _, _, " blue"), extract_num(X, Before, Num), order_selection(XS, [R, G, _]).
order_selection([X|XS], [R, Num, B]) :- sub_string(X, Before, _, _, " green"), extract_num(X, Before, Num), order_selection(XS, [R, _, B]).
order_selection([X|XS], [Num, G, B]) :- sub_string(X, Before, _, _, " red"), extract_num(X, Before, Num), order_selection(XS, [_, G, B]).

extract_num(Str, Len, Num) :- sub_string(Str, 0, Len, _, SubStr), number_string(Num, SubStr).

max_cubes(Cubes, MaxCubes) :- foldl(max, Cubes, [0, 0, 0], MaxCubes).

max([R, G, B], [R1, G1, B1], [Rmax, Gmax, Bmax]) :- Rmax is max(R, R1), Gmax is max(G, G1), Bmax is max(B, B1).

find_games([R, G, B], [Id0, Sum0], [Id, Sum]) :- R =< 12, G =< 13, B =< 14, Id is Id0 + 1, Sum is Sum0 + Id0.
find_games(_, [Id0, Sum0], [Id, Sum0]) :- Id is Id0 + 1.

find_powers([R, G, B], Sum0, Sum) :- Sum is Sum0 + R * G * B.
