:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(3605, 596734624269210).

solve(RC, F) :-
    read_file_to_lines_codes('../inputs/day21.txt', Lines),
    fold_grid(fold_cell, Lines, 0-0-[], SPos-PlotsList0),
    list_to_assoc(PlotsList0, Plots0),
    reachable(Plots0, 64, [SPos], RC),
    grid_size(Lines, W, W),
    % the following code to calculate part 2 is a burning pile of crap
    % and only works for carefully crafted inputs like the real one,
    % where there are free fields in a diamond shape, the main axes are free
    % and the number of steps is so, that the edge of a map is exactly reached.
    X is 26501365, Weven is W + 1,
    % how many maps in one direction will be fully covered minus starting map
    C is (X - W // 2) // W - 1,
    % how many even (C1) and odd (C2) maps will be fully covered
    C1 is (C + 1)**2, C2 is C**2,
    % how much is left walking after fully covered maps
    CC is (X - W // 2 - 1) mod W,
    CE is CC - W //2 - 1,
    reachable(Plots0, Weven, [SPos], RC1),
    reachable(Plots0, W, [SPos], RC2),
    spos_border(W, ST, SR, SB, SL),
    % corner partially reachable
    maplist(reachable(Plots0, CC), [[ST],[SR],[SB],[SL]], RCCS), sum_list(RCCS, RCC),
    % edge partially reachable (tow types)
    maplist(reachable(Plots0, CC), [[ST,SR],[ST,SL],[SB,SR],[SB,SL]], RCES), sum_list(RCES, RCE),
    maplist(reachable(Plots0, CE), [[1-1],[W-1],[1-W],[W-W]], RCES2), sum_list(RCES2, RCE2),
    F is C1 * RC1 + C2 * RC2 + RCC + RCE * C + RCE2 * (C + 1).

fold_cell(_, V, 0'#, V).
fold_cell(Pos, S-V0, 0'., S-[Pos-0|V0]).
fold_cell(Pos, _-V0, 0'S, Pos-[Pos-0|V0]).

spos_border(W, H-1, W-H, H-W, 1-H) :- H is W // 2 + 1.

reachable(Plots0, N, SPos, RC) :-
    reachable(Plots0, Plots, SPos, 0, N),
    assoc_to_list(Plots, PlotsList),
    Pairity is N mod 2,
    include(inc_plots(Pairity), PlotsList, RS), length(RS, RC).

reachable(Plots, Plots, [], _, _).
reachable(Plots, Plots, _, Max, Max).
reachable(Plots0, Plots, PS, N, Max) :-
    succ(N, N1),
    maplist(nexts(Plots0), PS, NS0), append(NS0, NS1), sort(NS1, NS),
    foldl(insert(N1), NS, Plots0, Plots1), reachable(Plots1, Plots, NS, N1, Max).

nexts(Plots, X-Y, NS) :-
    succ(X0, X), succ(X, X1), succ(Y0, Y), succ(Y, Y1),
    include(not_reached_plot(Plots), [X0-Y, X1-Y, X-Y0, X-Y1], NS).

not_reached_plot(Plots, X-Y) :- get_assoc(X-Y, Plots, 0).

insert(N, P, Plots0, Plots) :- get_assoc(P, Plots0, 0, Plots, N).

inc_plots(P, _-_-R) :- R =\= 0, R mod 2 =:= P.
