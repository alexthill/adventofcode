:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(_, _).

solve(Sum, _) :-
    read_file_to_lines_codes('../inputs/day03.txt', [L|LS]),
    augment_table([L|LS], Table),
    dot_row(L, Dots),
    parse_table([Dots|Table], NS),
    sum_list(NS, Sum).

parse_table([_,_], []).
parse_table([L0,L1,L2|LS], NS) :- parse([L0,L1,L2], NS0), parse_table([L1,L2|LS], NS1), append(NS0, NS1, NS).

parse([[_,_],[_,_],[_,_]], []).
parse([[L00,L01,L02|L0S],[L10,L11,L12|L1S],[L20,L21,L22|L2S]], NS) :-
    L11 >= 0'0, L11 =< 0'9, N is L11 - 0'0,
    is_any_symbol([L00,L01,L02,L10,L11,L12,L20,L21,L22], S),
    parse([[L01,L02|L0S],[L11,L12|L1S],[L21,L22|L2S]], N, NS, S).
parse([[_,L01,L02|L0S],[_,L11,L12|L1S],[_,L21,L22|L2S]], NS) :-
    parse([[L01,L02|L0S],[L11,L12|L1S],[L21,L22|L2S]], NS).

parse([[_,_],[_,_],[_,_]], _, [], false).
parse([[_,_],[_,_],[_,_]], N, [N], true).
parse([[L00,L01,L02|L0S],[L10,L11,L12|L1S],[L20,L21,L22|L2S]], N0, NS, false) :-
    L11 >= 0'0, L11 =< 0'9, N is N0 * 10 + L11 - 0'0,
    is_any_symbol([L00,L01,L02,L10,L11,L12,L20,L21,L22], S),
    parse([[L01,L02|L0S],[L11,L12|L1S],[L21,L22|L2S]], N, NS, S).
parse([[_,L01,L02|L0S],[_,L11,L12|L1S],[_,L21,L22|L2S]], N0, NS, true) :-
    L11 >= 0'0, L11 =< 0'9, N is N0 * 10 + L11 - 0'0,
    parse([[L01,L02|L0S],[L11,L12|L1S],[L21,L22|L2S]], N, NS, true).
parse([[_,L01,L02|L0S],[_,L11,L12|L1S],[_,L21,L22|L2S]], _, NS, false) :-
    parse([[L01,L02|L0S],[L11,L12|L1S],[L21,L22|L2S]], NS).
parse([[_,L01,L02|L0S],[_,L11,L12|L1S],[_,L21,L22|L2S]], N, [N|NS], true) :-
    parse([[L01,L02|L0S],[L11,L12|L1S],[L21,L22|L2S]], NS).

is_any_symbol([], false).
is_any_symbol([X|_], true) :- X \= 0'., (X < 0'0; X > 0'9).
is_any_symbol([_|XS], S) :- is_any_symbol(XS, S).

augment_table([L], [[0'.|A],Dots]) :- augment_row(L, A),  dot_row(L, Dots).
augment_table([L|LS], [[0'.|A]|AS]) :- augment_row(L, A), augment_table(LS, AS).

augment_row([], [0'.]).
augment_row([X|XS], [X|YS]) :- augment_row(XS, YS).

dot_row(L, Dots) :- same_length([0,0|L], Dots), maplist(=(0'.), Dots).