:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(532445, 79842967).

solve(Sum, Part2) :-
    read_file_to_lines_codes('../inputs/day03.txt', [L|LS]),
    augment_table([L|LS], Table),
    dot_row(L, Dots),
    parse_table([Dots|Table], NS),
    sum_list(NS, Sum),
    gear_ratios([L|LS], Ratios),
    sum_list(Ratios, Part2).

% Part 2 %

gear_ratios(Ls, Ys) :- build_num_map(Ls, A), gear_ratios(Ls, 1, A, Yss), flatten(Yss, Ys).
gear_ratios([], _, _, []).
gear_ratios([L|Ls], Y, A, [Ys|Yss]) :-
    gear_ratios_row(L, 1-Y, A, Ys),
    succ(Y, Y1),
    gear_ratios(Ls, Y1, A, Yss).

gear_ratios_row([], _, _, []).
gear_ratios_row([0'*|Cs], PX-PY, A, [R|Ys]) :-
    get_nums_around_gear(PX, PY, A, R),
    succ(PX, PX1),
    gear_ratios_row(Cs, PX1-PY, A, Ys).
gear_ratios_row([_|Cs], PX-PY, A, Ys) :- succ(PX, PX1), gear_ratios_row(Cs, PX1-PY, A, Ys).

get_nums_around_gear(X, Y, A, R) :-
    succ(X0, X), succ(X, X1), succ(Y0, Y), succ(Y, Y1),
    get_nums_around_gear([X0-Y0,X-Y0,X1-Y0,X0-Y,X-Y,X1-Y,X0-Y1,X-Y1,X1-Y1], A, Ns0),
    sort(Ns0, Ns),
    mult_ratios(Ns, A, R).
get_nums_around_gear([], _, []).
get_nums_around_gear([P|Ps], A, [X-Y|Ns]) :-
    get_assoc(P, A, X-Y-_),
    get_nums_around_gear(Ps, A, Ns).
get_nums_around_gear([_|Ps], A, Ns) :- get_nums_around_gear(Ps, A, Ns).

mult_ratios([P1,P2], A, R) :- get_assoc(P1, A, _-_-R1), get_assoc(P2, A, _-_-R2), R is R1 * R2.

% in Ls: input lines
% out A: assoc map with Pos-Num pairs indicating for each position by which number it is occupied
build_num_map(Ls, A) :- empty_assoc(A0), build_num_map(Ls, A0, A, 1).
build_num_map([], A, A, _).
build_num_map([L|Ls], A, A2, Y) :-
    find_nums_row(L, Ns),
    put_nums_in_map(Y, Ns, A, A1),
    succ(Y, Y1),
    build_num_map(Ls, A1, A2, Y1).

% in Y: row number
% in Ns: X-num-len triples from find_nums_row
% in A: associative map
% out A1: assoc with (X-Y)-(Xorg-Y_org-num) pairs added
put_nums_in_map(_, [], A, A).
put_nums_in_map(_-Y, [_-_-0|Ns], A, A1) :- put_nums_in_map(Y, Ns, A, A1).
put_nums_in_map(Xorg-Y, [X-N-L|Ns], A, A2) :-
    put_assoc(X-Y, A, Xorg-Y-N, A1),
    succ(L0, L), succ(X0, X),
    put_nums_in_map(Xorg-Y, [X0-N-L0|Ns], A1, A2).
put_nums_in_map(Y, [X-N-L|Ns], A, A1) :- put_nums_in_map(X-Y, [X-N-L|Ns], A, A1).

% in Xs: a string with any numbers inside
% out Ys: a list of triples pos-num-len for all numbers in Xs
find_nums_row(Xs, Ys) :- find_nums_row(Xs, Ys, 0).
find_nums_row([], [], _).
find_nums_row([X|Xs], Ys, P) :-
    X >= 0'0, X =< 0'9,
    succ(P, P1), N is X - 0'0,
    find_nums_row(Xs, Ys, P1, N, 1).
find_nums_row([_|Xs], Ys, P) :- succ(P, P1), find_nums_row(Xs, Ys, P1).
find_nums_row([], [P-N-L], P, N, L).
find_nums_row([X|Xs], Ys, P, N0, L0) :-
    X >= 0'0, X =< 0'9,
    succ(P, P1), succ(L0, L1), N1 is N0 * 10 + X - 0'0,
    find_nums_row(Xs, Ys, P1, N1, L1).
find_nums_row([_|Xs], [P-N-L|Ys], P, N, L) :- succ(P, P1), find_nums_row(Xs, Ys, P1).

% Part 1 %

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
