:- ensure_loaded(utils).
:- use_module(library(clpr)).
:- use_module(library(clpfd)).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(25261, _).

solve(C, Stone) :-
    read_file_to_lines_codes('../inputs/day24.txt', Lines),
    maplist(all_nums, Lines, Hails),
    Min is 7, Max is 27,
    %Min is 200000000000000, Max is 400000000000000,
    findall(A-B, (pair_from_list(Hails, A-B), intersect2d(Min, Max, A, B)), XS),
    length(XS, C),
    %maplist(intersect3d([24, 13, 10, -3, 1, 2]), Hails, Points).
    maplist(intersect3d(Stone), Hails, _).
    %part2(Hails, Stone).

intersect2d(Min, Max, [X1,Y1,_,VX1,VY1,_], [X2,Y2,_,VX2,VY2,_]) :-
    {
        X1 + VX1 * K1 = X2 + VX2 * K2,
        Y1 + VY1 * K1 = Y2 + VY2 * K2,
        X = X1 + VX1 * K1,
        Y = Y1 + VY1 * K1
    }, !,
    (   (X < Min ; X > Max ; Y < Min ; Y > Max)
    ->  false
    ;   (K1 < 0 ; K2 < 0)
    -> false
    ; true
    ).

part2([A,B|_], Stone) :- intersect3d(Stone, A, _), intersect3d(Stone, B, _).
intersect3d([X1,Y1,Z1,VX1,VY1,VZ1], [X2,Y2,Z2,VX2,VY2,VZ2], [T,X,Y,Z]) :-
        T #>= 0,
        X1 + VX1 * T #= X2 + VX2 * T,
        Y1 + VY1 * T #= Y2 + VY2 * T,
        Z1 + VZ1 * T #= Z2 + VZ2 * T,
        X #= X1 + VX1 * T,
        Y #= Y1 + VY1 * T,
        Z #= Z1 + VZ1 * T.

pair_from_list([X|XS], P) :- pair_to(X, XS, P).
pair_from_list([_|XS], P) :- pair_from_list(XS, P).

pair_to(X, [Y|_], X-Y).
pair_to(X, [_|YS], P) :- pair_to(X, YS, P).
