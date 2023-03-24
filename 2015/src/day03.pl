main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(2565, 2639).

solve(HouseCount, HouseCount2) :-
    read_file_to_string('../inputs/day03.txt', Str, []),
    string_chars(Str, Chars),
    houses(Chars, [0, 0], Pos),
    sort(Pos, Houses),
    length(Houses, HouseCount),
    houses2(Chars, [0, 0], [0, 0], Pos2),
    sort(Pos2, Houses2),
    length(Houses2, HouseCount2).

houses([], P, [P]).
houses([C|CS], P0, [P0|PS]) :- update_pos(C, P0, P), houses(CS, P, PS).

houses2([], SP, RP, [SP,RP]).
houses2([SC,RC|CS], SP0, RP0, [SP0,RP0|PS]) :- update_pos(SC, SP0, SP), update_pos(RC, RP0, RP), houses2(CS, SP, RP, PS).

update_pos('>', [X0, Y], [X, Y]) :- X is X0 + 1.
update_pos('<', [X0, Y], [X, Y]) :- X is X0 - 1.
update_pos('^', [X, Y0], [X, Y]) :- Y is Y0 + 1.
update_pos('v', [X, Y0], [X, Y]) :- Y is Y0 - 1.
