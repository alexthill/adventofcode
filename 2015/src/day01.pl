main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(232, 1783).

solve(Floor, Basement) :-
    read_file_to_string('../inputs/day01.txt', Str, []),
    string_chars(Str, Chars),
    floor(0, Chars, Floor, 0, Basement).

floor(F, [], F, _, _).
floor(F0, ['('|CS], F, C0, B) :- C is C0 + 1, F1 is F0 + 1, floor(F1, CS, F, C, B).
floor( 0, [')'|CS], F, C0, C) :- C is C0 + 1, floor(-1, CS, F, C, C).
floor(F0, [')'|CS], F, C0, B) :- C is C0 + 1, F1 is F0 - 1, floor(F1, CS, F, C, B).
