:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(15517, 14935034899483).

solve(C, C2) :-
    read_file_to_lines_string('../inputs/day08.txt', [Dirs0,_|Lines]),
    string_codes(Dirs0, Dirs),
    maplist(parse_line, Lines, Locs),
    list_to_assoc(Locs, LocMap),
    steps(LocMap, Dirs, `AAA`, Dirs, C),
    include(is_start, Locs, StartLocs),
    pairs_keys(StartLocs, Starts),
    maplist(steps2(LocMap, Dirs), Starts, CS),
    lcm_of_list(CS, C2).

parse_line(Line, F1-[L1,R1]) :-
    sub_string(Line, 0, 3, _, F), string_codes(F, F1),
    sub_string(Line, 7, 3, _, L), string_codes(L, L1),
    sub_string(Line, 12, 3, _, R), string_codes(R, R1).

steps(_, _, `ZZZ`, _, 0).
steps(LocMap, Dirs, Loc, [], C) :- steps(LocMap, Dirs, Loc, Dirs, C).
steps(LocMap, Dirs, Loc, [0'L|DS], C) :- get_assoc(Loc, LocMap, [Loc1,_]), steps(LocMap, Dirs, Loc1, DS, C0), succ(C0, C).
steps(LocMap, Dirs, Loc, [0'R|DS], C) :- get_assoc(Loc, LocMap, [_,Loc1]), steps(LocMap, Dirs, Loc1, DS, C0), succ(C0, C).

steps2(LocMap, Dirs, Start, C) :- steps2(LocMap, Dirs, Start, Dirs, C).
steps2(_, _, [_,_,0'Z], [], 0).
steps2(LocMap, Dirs, Loc, [], C) :- steps2(LocMap, Dirs, Loc, Dirs, C).
steps2(LocMap, Dirs, Loc, [0'L|DS], C) :- get_assoc(Loc, LocMap, [Loc1,_]), steps2(LocMap, Dirs, Loc1, DS, C0), succ(C0, C).
steps2(LocMap, Dirs, Loc, [0'R|DS], C) :- get_assoc(Loc, LocMap, [_,Loc1]), steps2(LocMap, Dirs, Loc1, DS, C0), succ(C0, C).

is_start([_,_,0'A]-_).

gcd(X, 0, X) :- !.
gcd(X, Y, Z) :- H is X rem Y, gcd(Y, H, Z).

lcm(X,Y,LCM) :- gcd(X,Y,GCD), LCM is X*Y//GCD.

lcm_of_list([],1) :- !.
lcm_of_list([X|Xs],LCM) :- lcm_of_list(Xs,LCM2), lcm(X,LCM2,LCM).
