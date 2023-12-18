:- ensure_loaded(utils).
:- use_module(library(clpfd)).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(110090, 95254).

solve(Load, Load2) :-
    read_file_to_lines_codes('../inputs/day14.txt', Lines),
    transpose(Lines, LinesT),
    maplist(tilt, LinesT, LinesTT),
    sum_loads(LinesTT, Load),
    empty_assoc(A),
    run_cycles(Lines, A, 0, L),
    transpose(L, LT),
    sum_loads(LT, Load2).

run_cycles(XS, A, C, L) :- get_assoc(XS, A, C0), N is ((1000000000 - C0) mod (C - C0)) * 4, run_cycle(XS, L, N).
run_cycles(XS, A, C, L) :- succ(C, C1), put_assoc(XS, A, C, A1), run_cycle(XS, YS, 4), run_cycles(YS, A1, C1, L).

run_cycle(XS, XS, 0).
run_cycle(XS, YS, N) :-
    transpose(XS, TS),
    maplist(tilt, TS, TTS),
    maplist(reverse, TTS, TTRS),
    succ(N0, N), run_cycle(TTRS, YS, N0).

tilt(XS, YS) :- tilt(XS, YS, 0, 0).
tilt([], YS, CE, CR) :- roll_rocks(CE, CR, ES, RS), append(RS, ES, YS).
tilt([0'O|XS], YS, CE, CR) :- succ(CR, CR1), tilt(XS, YS, CE, CR1).
tilt([0'.|XS], YS, CE, CR) :- succ(CE, CE1), tilt(XS, YS, CE1, CR).
tilt([0'#|XS], YS, CE, CR) :- roll_rocks(CE, CR, ES, RS), tilt(XS, YS0, 0, 0), append([RS, ES, [0'#], YS0], YS).

roll_rocks(CE, CR, ES, RS) :- length(ES, CE), maplist(=(0'.), ES), length(RS, CR), maplist(=(0'O), RS).

sum_loads(XS, L) :- maplist(reverse, XS, RS), maplist(load(1), RS, LS), sum_list(LS, L).

load(_, [], 0).
load(N, [0'O|XS], L) :- succ(N, N0), load(N0, XS, L0), L is N + L0.
load(N, [_|XS], L) :- succ(N, N0), load(N0, XS, L).