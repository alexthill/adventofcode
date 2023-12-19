:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(48400, 72811019847283).

solve(Area1, Area2) :-
    read_file_to_lines_string('../inputs/day18.txt', Lines),
    maplist(parse_line, Lines, Rows),
    pairs_keys_values(Rows, Instrc1, Hexs),
    area(Instrc1, Area1),
    maplist(parse_hex, Hexs, Instrc2),
    area(Instrc2, Area2).

parse_line(Line, Dir-Num-Hex) :- split_string(Line, " ", "()", [Dir, NumStr, Hex]), number_string(Num, NumStr).

area(Instrc, A) :- scanl(dig, Instrc, 0-0, ES), shoelace(ES, A0), edge_len(ES, B), A is A0 / 2 + B / 2 + 1.

dig("U"-N, X-Y0, X-Y) :- Y is Y0 - N.
dig("D"-N, X-Y0, X-Y) :- Y is Y0 + N.
dig("L"-N, X0-Y, X-Y) :- X is X0 - N.
dig("R"-N, X0-Y, X-Y) :- X is X0 + N.

shoelace([_], 0).
shoelace([X1-Y1,X2-Y2|ES], S) :- shoelace([X2-Y2|ES], S0), S is S0 + X1 * Y2 - X2 * Y1.

edge_len([_], 0).
edge_len([X1-Y1,X2-Y2|ES], L) :- edge_len([X2-Y2|ES], L0), L is L0 + abs(X1 - X2) + abs(Y1 - Y2).

parse_hex(Hex, Dir-N) :-
    string_codes(Hex, [_,A,B,C,D,E,F]),
    number_codes(N, [0'0,0'x,A,B,C,D,E]),
    num_to_dir(F, Dir).

num_to_dir(0'0, "R").
num_to_dir(0'1, "D").
num_to_dir(0'2, "L").
num_to_dir(0'3, "U").
