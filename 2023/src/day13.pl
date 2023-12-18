:- ensure_loaded(utils).
:- use_module(library(clpfd)).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(30575, 37478).

solve(N, N2) :-
    read_file_to_lines_codes('../inputs/day13.txt', Lines),
    split_on_empty_lines(Lines, PS),
    maplist(find_sym(find_sym_hor), PS, NS),
    sum_list(NS, N),
    maplist(find_sym(find_sym_hor2), PS, NS2),
    sum_list(NS2, N2).

find_sym(Fn, LS, N) :- (call(Fn, LS, N) ; transpose(LS, LST), call(Fn, LST, N0), N is N0 * 100).

find_sym_hor([L|LS], N) :- length(L, Len), succ(L0, Len), between(1, L0, N), maplist(line_sym_at(N), [L|LS]).

find_sym_hor2([L|LS], N) :- length(L, Len), succ(L0, Len), between(1, L0, N), one_line_one_dif(N, [L|LS]).

line_sym_at(N, Line) :- split_at_n(N, Line, AS, BS), reverse(AS, AR), begin_equals(AR, BS).

begin_equals([], _).
begin_equals(_, []).
begin_equals([A|AS], [A|BS]) :- begin_equals(AS, BS).

split_at_n(_, [], []).
split_at_n(0, XS, [], XS).
split_at_n(_, [], [], []).
split_at_n(N, [X|XS], [X|AS], BS) :- succ(N0, N), split_at_n(N0, XS, AS, BS).

one_line_one_dif(N, [L|LS]) :- line_sym_at(N, L), one_line_one_dif(N, LS).
one_line_one_dif(N, [L|LS]) :- split_at_n(N, L, AS, BS), reverse(AS, AR), exactly_one_dif(AR, BS), maplist(line_sym_at(N), LS).

exactly_one_dif([A|AS], [A|BS]) :- exactly_one_dif(AS, BS).
exactly_one_dif([A|AS], [B|BS]) :- dif(A, B), begin_equals(AS, BS).

split_on_empty_lines([], [[]]).
split_on_empty_lines([[]|LS], [[]|PS]) :- split_on_empty_lines(LS, PS).
split_on_empty_lines([L|LS], [[L|P]|PS]) :- split_on_empty_lines(LS, [P|PS]).
