:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(522547, 229271).

solve(Sum, P) :-
    read_file_to_string('../inputs/day15.txt', Seq, []),
    split_string(Seq, ",", "\n", Steps),
    maplist(hashr, Steps, HS),
    sum_list(HS, Sum),
    list_between(0, 255, BSL), list_to_assoc(BSL, BS0),
    apply_steps(Steps, BS0, BS),
    assoc_to_list(BS, BSList),
    maplist(foc_power, BSList, PS),
    sum_list(PS, P).

hashr(X, H) :- string_codes(X, Y), reverse(Y, R), hash(R, H).

hash([], 0).
hash([X|XS], H) :- hash(XS, H0), H is ((H0 + X) * 17) mod 256.

apply_steps([], BS, BS).
apply_steps([S|SS], BS0, BS) :-
    split_string(S, "-", "", [L,""]),
    hashr(L, H), get_assoc(H, BS0, B0, BS1, B), subtract(B0, [L-_], B),
    apply_steps(SS, BS1, BS).
apply_steps([S|SS], BS0, BS) :-
    split_string(S, "=", "", [L,F]),
    number_string(FN, F),
    hashr(L, H), get_assoc(H, BS0, B0, BS1, B), replace_lense(L-FN, B0, B),
    apply_steps(SS, BS1, BS).

replace_lense(L, [], [L]).
replace_lense(L-F, [L-_|B], [L-F|B]).
replace_lense(L, [X|B0], [X|B]) :- replace_lense(L, B0, B).

list_between(S, S, [S-[]]).
list_between(S, E, [S-[]|XS]) :- succ(S, S1), list_between(S1, E, XS).

foc_power(BN-B, P) :- foc_power(1, BN, B, P).
foc_power(_, _, [], 0).
foc_power(SN, BN, [_-F|B], P) :- succ(SN, SN1), foc_power(SN1, BN, B, P0), P is (BN + 1) * SN * F + P0.
