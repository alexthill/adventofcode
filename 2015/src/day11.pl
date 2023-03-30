main :-
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions("vzbxxyzz", "vzcaabcc").

solve(P1, P2) :-
    read_file_to_codes('../inputs/day11.txt', Codes, []),
    no_iol(Codes, Codes0),
    next_pass(Codes0, Codes1), string_codes(P1, Codes1),
    next_pass(Codes1, Codes2), string_codes(P2, Codes2).

next_pass(X, Y) :- augment(X, X1), no_iol(X1, X2), (validate_double(X2), validate_row(X2) -> return_first(X2, 0, Y); next_pass(X2, Y)).

augment([0'z, 0'z, 0'z, 0'z, 0'z, 0'z, 0'z, 0'z], _) :- writeln("whoopsie, no solution"), false.
augment([X, 0'z, 0'z, 0'z, 0'z, 0'z, 0'z, 0'z], [Y, 0'a, 0'a, 0'a, 0'a, 0'a, 0'a, 0'a]) :- Y is X + 1.
augment([A, X, 0'z, 0'z, 0'z, 0'z, 0'z, 0'z], [A, Y, 0'a, 0'a, 0'a, 0'a, 0'a, 0'a]) :- Y is X + 1.
augment([A, B, X, 0'z, 0'z, 0'z, 0'z, 0'z], [A, B, Y, 0'a, 0'a, 0'a, 0'a, 0'a]) :- Y is X + 1.
augment([A, B, C, X, 0'z, 0'z, 0'z, 0'z], [A, B, C, Y, 0'a, 0'a, 0'a, 0'a]) :- Y is X + 1.
augment([A, B, C, D, X, 0'z, 0'z, 0'z], [A, B, C, D, Y, 0'a, 0'a, 0'a]) :- Y is X + 1.
augment([A, B, C, D, E, X, 0'z, 0'z], [A, B, C, D, E, Y, 0'a, 0'a]) :- Y is X + 1.
augment([A, B, C, D, E, F, X, 0'z], [A, B, C, D, E, F, Y, 0'a]) :- Y is X + 1.
augment([A, B, C, D, E, F, G, X], [A, B, C, D, E, F, G, Y]) :- Y is X + 1.

no_iol([], []).
no_iol([0'i|XS], [0'j|YS]) :- maplist(return_first(0'a), XS, YS).
no_iol([0'o|XS], [0'p|YS]) :- maplist(return_first(0'a), XS, YS).
no_iol([0'l|XS], [0'm|YS]) :- maplist(return_first(0'a), XS, YS).
no_iol([X|XS], [X|YS]) :- no_iol(XS, YS).

return_first(A, _, A).

validate_double([X,X|XS], X) :- validate_double(XS, X).
validate_double([X,X|_], _).
validate_double([_|XS], D) :- validate_double(XS, D).

validate_double([X,X|XS]) :- validate_double(XS, X).
validate_double([_|XS]) :- validate_double(XS).

validate_row([X,Y,Z|_]) :- Y =:= X + 1, Z =:= Y + 1.
validate_row([_|XS]) :- validate_row(XS).
