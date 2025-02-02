:- ensure_loaded(utils).

% The solution for part 2 requires the input to be in a very specific form
% with a conjunction module going to rx, that has exactly 4 inputs.
% These 4 inputs must be cyclic and the cycles must be co-prime and no cycle
% can repeat in the time it takes an other cycle to finish once.
main :-
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(898731036, 229414480926893).

solve(Res, Res2) :-
    read_file_to_lines_string('../inputs/day20.txt', Lines),
    maplist(parse_module, Lines, ModsList),
    list_to_assoc(ModsList, Mods0),
    init_conjs(ModsList, Mods0, Mods),
    include(has_ancestor(rx), ModsList, [Anc-_]),
    repeat([]-Anc-1, _, 1000, Mods, _, LC-HC), Res is LC * HC,
    repeat2([]-Anc-1, Xs-_-_, Mods, _), foldl(product, Xs, 1, Res2).

product(X, Y, Z) :- Z is X * Y.

% Parsing %

init_conjs([], MS, MS).
init_conjs([N-M|LS], MS0, MS) :-
    foldl(update_conj(N), M.dest, MS0, MS1),
    init_conjs(LS, MS1, MS).

update_conj(F, N, MS0, MS) :-
    get_assoc(N, MS0, M0), M0.type = conj,
    put_assoc(F, M0.val, 0, Val), M = M0.put(val, Val),
    put_assoc(N, MS0, M, MS).
update_conj(_, _, MS, MS).

parse_module(Line, Name-module{type:T, dest:DS, val:V, name:Name}) :-
    split_string(Line, ">", " -", [A,B]),
    string_codes(A, Name0), module_type(Name0, Name1, T, V), atom_codes(Name, Name1),
    split_string(B, ",", " ", DS0), maplist(atom_string, DS, DS0).

module_type([0'%|Name], Name, flip, 0).
module_type([0'&|Name], Name, conj, A) :- empty_assoc(A).
module_type(Name, Name, norm, x).

% Part 2 %

has_ancestor(Anc, _-module{type:_, dest:[Anc], val:_, name:_}).

repeat2(IO-9000, IO-9000, _, _) :- writeln('failed to find solution within given limit').
repeat2(I, O, Ms0, Ms1) :-
    pulse(I, OI, Ms0, Ms, _), !,
    (   OI = Xs-_-_, length(Xs, 4)
    ->  O = OI, Ms1 = Ms
    ;   repeat2(OI, O, Ms, Ms1)
    ).

% Part 1 %

repeat(I, I, 0, Ms, Ms, 0-0).
repeat(I, O, N, Ms0, Ms1, LC-HC) :-
    pulse(I, OI, Ms0, Ms, LC0-HC0), !,
    succ(N0, N), repeat(OI, O, N0, Ms, Ms1, LC1-HC1),
    HC is HC0 + HC1, LC is LC0 + LC1.

pulse(I, O, MS0, MS, CS) :- pulse(I, O, MS0, [broadcaster-0-x], MS, CS).
pulse(IO-N, IO-N1, MS, [], MS, 0-0) :- succ(N, N1).
pulse(I, O, MS0, [MN-P-F|Q], MS, LC-HC) :-
    (   get_assoc(MN, MS0, M)
    ->  next_pulses(I, OI, M, P, F, M1, PS),
        append(Q, PS, Q1),
        put_assoc(MN, MS0, M1, MS1)
    ;   Q1 = Q, MS1 = MS0, OI = I % MN is rx if this else executes
    ),
    pulse(OI, O, MS1, Q1, MS, LC0-HC0),
    (P = 0 -> succ(LC0, LC), HC = HC0 ; succ(HC0, HC), LC = LC0).

next_pulses(IO, IO, M, 1, _, M, []) :- M.type = flip.
next_pulses(IO, IO, M0, 0, _, M, NPS) :-
    M0.type = flip, get_dict(val, M0, V0, M, V),
    V is V0 xor 1, maplist(dest_to_next(M.name, V), M.dest, NPS).
next_pulses(I-Anc-N, O-Anc-N, M0, P, F, M, NPS) :-
    M0.type = conj, put_assoc(F, M0.val, P, Val), M = M0.put(val, Val),
    (map_assoc(=(1), M.val) -> V = 0; V = 1), maplist(dest_to_next(M.name, V), M.dest, NPS),
    (M0.name = Anc, P = 1 -> O = [N|I]; O = I).
next_pulses(IO, IO, M, P, _, M, NPS) :-
    M.type = norm, maplist(dest_to_next(M.name, P), M.dest, NPS).

dest_to_next(F, P, D, D-P-F).
