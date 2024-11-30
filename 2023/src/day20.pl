:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(898731036, _).

solve(Res, Ancs-NS) :-
    read_file_to_lines_string('../inputs/day20.txt', Lines),
    maplist(parse_module, Lines, ModsList),
    list_to_assoc(ModsList, Mods0),
    init_conjs(ModsList, Mods0, Mods),
    repeat(1, Mods, LC-HC), Res is LC * HC,
    gen_assoc(Anc, Mods, module{type:_, dest:[rx], val:_, name:_}),
    findall(X, gen_assoc(X, Mods, module{type:_, dest:[Anc], val:_, name:_}), Ancs),
    !, maplist(conj_low(Mods), Ancs, NS).

parse_module(Line, Name-module{type:T, dest:DS, val:V, name:Name}) :-
    split_string(Line, ">", " -", [A,B]),
    string_codes(A, Name0), module_type(Name0, Name1, T, V), atom_codes(Name, Name1),
    split_string(B, ",", " ", DS0), maplist(atom_string, DS, DS0).

module_type([0'%|Name], Name, flip, 0).
module_type([0'&|Name], Name, conj, A) :- empty_assoc(A).
module_type(Name, Name, norm, x).

init_conjs([], MS, MS).
init_conjs([N-M|LS], MS0, MS) :- foldl(update_conj(N), M.dest, MS0, MS1), init_conjs(LS, MS1, MS).

update_conj(F, N, MS0, MS) :-
    get_assoc(N, MS0, M0), M0.type = conj,
    put_assoc(F, M0.val, 0, Val), M = M0.put(val, Val),
    put_assoc(N, MS0, M, MS).
update_conj(_, _, MS, MS).

repeat(0, _, 0-0).
repeat(N, MS0, LC-HC) :- pulse(MS0, MS, LC0-HC0), !, succ(N0, N), repeat(N0, MS, LC1-HC1), HC is HC0 + HC1, LC is LC0 + LC1.

conj_low(MS0, CJ, N) :-
    pulse(MS0, MS, _), !,
    (
        get_assoc(CJ, MS0, M), gen_assoc(Anc, M.val, 0), get_assoc(Anc, MS0, M2), writeln(Anc), !, maplist(=(1), M2.val)
    ->  N is 1, writeln(M), get_assoc(ns, MS0, M2), writeln(M2)
    ;   conj_low(MS, CJ, N0), succ(N0, N)
    ).

pulse(MS0, MS, CS) :- pulse(MS0, [broadcaster-0-x], MS, CS).
pulse(MS, [], MS, 0-0).
pulse(MS, [rx-0-_|_], MS, 0-0).
pulse(MS0, [MN-P-F|Q], MS, LC-HC) :-
    %writeln(F-P-MN),
    (   get_assoc(MN, MS0, M)
    ->  next_pulses(M, P, F, M1, PS),
        append(Q, PS, Q1),
        get_assoc(MN, MS0, M, MS1, M1)
    ;   Q1 = Q, MS1 = MS0
    ),
    !, pulse(MS1, Q1, MS, LC0-HC0),
    (P = 0 -> succ(LC0, LC), HC = HC0 ; succ(HC0, HC), LC = LC0).

next_pulses(M, 1, _, M, []) :- M.type = flip.
next_pulses(M0, 0, _, M, NPS) :-
    M0.type = flip, get_dict(val, M0, V0, M, V),
    V is V0 xor 1, maplist(dest_to_next(M.name, V), M.dest, NPS).
next_pulses(M0, P, F, M, NPS) :-
    M0.type = conj, put_assoc(F, M0.val, P, Val), M = M0.put(val, Val),
    (map_assoc(=(1), M.val) -> V = 0 ; V = 1), maplist(dest_to_next(M.name, V), M.dest, NPS).
next_pulses(M, P, _, M, NPS) :-
    M.type = norm, maplist(dest_to_next(M.name, P), M.dest, NPS).

dest_to_next(F, P, D, D-P-F).
