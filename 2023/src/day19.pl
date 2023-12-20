:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(532551, 134343280273968).

solve(Sum, Poss) :-
    read_file_to_lines_string('../inputs/day19.txt', Lines),
    foldl(parse, Lines, 0-[]-[], 1-RS0-PS),
    list_to_assoc(RS0, RS),
    maplist(apply_rules(RS, "in"), PS, Acc),
    foldl(sum_acc, Acc, PS, 0, Sum),
    apply_rules2(RS, "in", part{x:1-4000, m:1-4000, a:1-4000, s:1-4000}, APS0),
    flatten(APS0, APS), foldl(sum_acc2, APS, 0, Poss).

sum_acc(false, _, V, V).
sum_acc(true, P, V0, V) :- V is V0 + P.x + P.m + P.a + P.s.

sum_acc2(P, V0, V) :- foldl(part_poss(P), [x,m,a,s], 1, Poss), V is V0 + Poss.

part_poss(_, [], V, V).
part_poss(P, C, V0, V) :- get_dict(C, P, Min-Max), V is V0 * (Max - Min + 1).

parse("", 0-RS-PS, 1-RS-PS).
parse(Line, 0-RS-PS, 0-[Name-R|RS]-PS) :- split_string(Line, "{,:", "}", [Name|R0]), parse_rule(R0, R).
parse(Line, 1-RS-PS, 1-RS-[P|PS]) :- split_string(Line, ",", "{}", P0), maplist(str_to_pair, P0, P1), dict_create(P, part, P1).

parse_rule([X], [X]).
parse_rule([A,B|XS], [C-gt-V-B|YS]) :-
    split_string(A, ">", "", [C0,V0]),
    atom_string(C, C0), number_string(V, V0),
    parse_rule(XS, YS).
parse_rule([A,B|XS], [C-lt-V-B|YS]) :-
    split_string(A, "<", "", [C0,V0]),
    atom_string(C, C0), number_string(V, V0),
    parse_rule(XS, YS).

str_to_pair(S, A-B) :- split_string(S, "=", "", [A0,B0]), atom_string(A, A0), number_string(B, B0).

apply_rules(RS, RN, P, Status) :-
    get_assoc(RN, RS, R), apply_rule(R, P, Res),
    (Res = "A" -> Status = true ; (Res = "R" -> Status = false ; atom_string(Res, RN1), apply_rules(RS, RN1, P, Status))).

apply_rule([C-lt-V-RN|_], P, RN) :- P.C < V.
apply_rule([C-gt-V-RN|_], P, RN) :- P.C > V.
apply_rule([_|XS], P, RN) :- apply_rule(XS, P, RN).
apply_rule([X], _, X).

apply_rules2(RS, RN, P, Status) :-
    get_assoc(RN, RS, R), apply_rule2(R, P, Res), maplist(check_status(RS), Res, Status).
    
check_status(RS, P-RN, Status) :-
    (RN = "A" -> Status = [P] ; (RN = "R" -> Status = [] ; atom_string(RN, RN1), apply_rules2(RS, RN1, P, Status))).

apply_rule2([C-lt-V-RN|_], P, [P-RN]) :- get_dict(C, P, _-Max), Max < V.
apply_rule2([C-lt-V-_|XS], P, YS) :- get_dict(C, P, Min-_), Min > V, apply_rule(XS, P, YS).
apply_rule2([C-gt-V-RN|_], P, [P-RN]) :- get_dict(C, P, Min-_), Min > V.
apply_rule2([C-gt-V-_|XS], P, YS) :- get_dict(C, P, _-Max), Max < V, apply_rule(XS, P, YS).
apply_rule2([C-Rel-V-RN|XS], P, [P1-RN|YS]) :- split_part(P, C, Rel, V, [P1,P2]), apply_rule2(XS, P2, YS).
apply_rule2([X], P, [P-X]).

split_part(P, C, lt, V, [P1,P2]) :- get_dict(C, P, Min-Max), succ(V0, V), P1 = P.put(C, Min-V0), P2 = P.put(C, V-Max).
split_part(P, C, gt, V, [P2,P1]) :- get_dict(C, P, Min-Max), succ(V, V1), P1 = P.put(C, Min-V), P2 = P.put(C, V1-Max).
