main :-
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(3176, 14710).

solve(P1, P2) :-
    open('../inputs/day07.txt', read, Str), read_file(Str, Rules), close(Str),
    resolve_rules_all(Rules, RR), last(RR, ["a", P1]),
    override("b", P1, Rules, Rules2),
    resolve_rules_all(Rules2, RR2), last(RR2, ["a", P2]).

read_file(Stream, []) :- at_end_of_stream(Stream).
read_file(Stream, [X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, String),
    split_string(String, " ", " ->", Subs),
    try_to_num(Subs, X),
    read_file(Stream, L).

try_to_num([], []).
try_to_num([X|XS], [N|NS]) :- try_to_num(XS, NS), number_string(N, X).
try_to_num([X|XS], [X|NS]) :- try_to_num(XS, NS).

resolve_rules_all([], []).
resolve_rules_all(RS, RR) :-
    resolve_rules(RS, UR, RR0),
    (length(RR0, 0) -> RR = [];  maplist(apply_all(RR0), UR, AUR), resolve_rules_all(AUR, RR1), append(RR0, RR1, RR)).

resolve_rules([], [], []).
resolve_rules([R|RS], UR, [[X, S]|RR]) :- resolve(R, X, S), resolve_rules(RS, UR, RR).
resolve_rules([R|RS], [R|UR], RR) :- resolve_rules(RS, UR, RR).
        
resolve([A, X], X, A) :- number(A).
resolve(["NOT", A, X], X, S) :- number(A), S is (\ A) /\ (2**16 - 1).
resolve([A,    "AND", B, X], X, S) :- number(A), number(B), S is A /\ B.
resolve([A,     "OR", B, X], X, S) :- number(A), number(B), S is A \/ B.
resolve([A, "RSHIFT", B, X], X, S) :- number(A), number(B), S is A >> B.
resolve([A, "LSHIFT", B, X], X, S) :- number(A), number(B), S is (A << B)  /\ (2**16 - 1).

apply(X, S, [X, Y], [S, Y]).
apply(X, S, [Op, X, Y], [Op, S, Y]).
apply(X, S, [X, Op, X, Y], [S, Op, S, Y]).
apply(X, S, [X, Op, B, Y], [S, Op, B, Y]).
apply(X, S, [A, Op, X, Y], [A, Op, S, Y]).
apply(_, _, R, R).

apply_all([], R, R).
apply_all([[X, S]|XS], R0, R) :- apply_all(XS, R0, R1), apply(X, S, R1, R).

override(K, V, [[_, K]|XS], [[V, K]|XS]).
override(K, V, [X|XS], [X|YS]) :- override(K, V, XS, YS).
