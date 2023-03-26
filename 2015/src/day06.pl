:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(543903, 14687245).

solve(P1, P2) :-
    open('../inputs/day06.txt', read, Str), read_file(Str, Rules), close(Str),
    reverse(Rules, Rules2),
    bagof([X, Y], (between(0, 999, X), between(0, 999, Y)), Coords),
    include(is_on(Rules2), Coords, OnList), length(OnList, P1), 
    maplist(brightness(Rules2), Coords, BrightnessList), sum_list(BrightnessList, P2).

read_file(Stream, []) :- at_end_of_stream(Stream).
read_file(Stream, [[T|Nums]|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, String),
    (
        sub_string(String, 0, _, _, "turn on") -> T = on;
        sub_string(String, 0, _, _, "turn off") -> T = off;
        T = toggle
    ),
    string_codes(String, Codes),
    all_nums(Codes, Nums),
    read_file(Stream, L).

is_on(Rules, [X, Y]) :- is_on(Rules, X, Y, true).
is_on([], _, _, false).
is_on([[on,X1,Y1,X2,Y2]|Rules], X, Y, true) :- between(X1, X2, X), between(Y1, Y2, Y); is_on(Rules, X, Y, true).
is_on([[on,X1,Y1,X2,Y2]|Rules], X, Y, false) :- between(X1, X2, X), between(Y1, Y2, Y) -> false; is_on(Rules, X, Y, false).
is_on([[off,X1,Y1,X2,Y2]|Rules], X, Y, true) :- between(X1, X2, X), between(Y1, Y2, Y) -> false; is_on(Rules, X, Y, true).
is_on([[off,X1,Y1,X2,Y2]|Rules], X, Y, false) :- between(X1, X2, X), between(Y1, Y2, Y); is_on(Rules, X, Y, false).
is_on([[toggle,X1,Y1,X2,Y2]|Rules], X, Y, true) :- between(X1, X2, X), between(Y1, Y2, Y) -> is_on(Rules, X, Y, false); is_on(Rules, X, Y, true).
is_on([[toggle,X1,Y1,X2,Y2]|Rules], X, Y, false) :- between(X1, X2, X), between(Y1, Y2, Y) -> is_on(Rules, X, Y, true); is_on(Rules, X, Y, false).

brightness(Rules, [X, Y], B) :- brightness(Rules, X, Y, B).
brightness([], _, _, 0).
brightness([[on,X1,Y1,X2,Y2]|Rules], X, Y, B) :- brightness(Rules, X, Y, B0), (between(X1, X2, X), between(Y1, Y2, Y) -> B is B0 + 1; B is B0).
brightness([[off,X1,Y1,X2,Y2]|Rules], X, Y, B) :- brightness(Rules, X, Y, B0), (between(X1, X2, X), between(Y1, Y2, Y), B0 > 0 -> B is B0 - 1; B is B0).
brightness([[toggle,X1,Y1,X2,Y2]|Rules], X, Y, B) :- brightness(Rules, X, Y, B0), (between(X1, X2, X), between(Y1, Y2, Y) -> B is B0 + 2; B is B0).
