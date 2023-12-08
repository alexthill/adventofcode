:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(251287184, 250757288).

solve(Winnings, Winnings2) :-
    read_file_to_lines_codes('../inputs/day07.txt', Lines),
    maplist(parse_line, Lines, Cards, Hands),
    sort(1, @=<, Cards, CardsSorted),
    foldl(fold_cards, CardsSorted, [1, 0], [_, Winnings]),
    sort(1, @=<, Hands, HandsSorted),
    foldl(fold_cards, HandsSorted, [1, 0], [_, Winnings2]).

parse_line([A,B,C,D,E,_|Bid0], [Val, Bid], [Val2, Bid]) :-
    number_codes(Bid, Bid0),
    hand_value([A,B,C,D,E], 0, V),
    hand_val(V, [A,B,C,D,E], Val),
    hand_value2([A,B,C,D,E], 0, V2),
    hand_combs(V2, [A,B,C,D,E], [], MaxHand),
    hand_val(V2, MaxHand, Val2).
    
fold_cards([_,Bid], [N0, Sum0], [N, Sum]) :- Sum is Sum0 + Bid * N0, succ(N0, N).

hand_val(V, H, Val) :- msort(H, SH), hand_type(SH, T), Val is T * 100000000000 + V.

hand_type(H, T) :- hand_type_h(H, T), !.
hand_type_h([A,A,A,A,A], 9).
hand_type_h([A,A,A,A,_], 8).
hand_type_h([_,A,A,A,A], 8).
hand_type_h([A,A,A,B,B], 7).
hand_type_h([B,B,A,A,A], 7).
hand_type_h([_,_,A,A,A], 6).
hand_type_h([_,A,A,A,_], 6).
hand_type_h([A,A,A,_,_], 6).
hand_type_h([A,A,B,B,_], 5).
hand_type_h([A,A,_,B,B], 5).
hand_type_h([_,A,A,B,B], 5).
hand_type_h([_,_,_,A,A], 4).
hand_type_h([_,_,A,A,_], 4).
hand_type_h([_,A,A,_,_], 4).
hand_type_h([A,A,_,_,_], 4).
hand_type_h(_, 3).

map_rank(0'T, 10).
map_rank(0'J, 11).
map_rank(0'Q, 12).
map_rank(0'K, 13).
map_rank(0'A, 14).
map_rank(C, N) :- N is C - 0'0.

hand_value([], V, V).
hand_value([X|XS], V0, V) :- map_rank(X, R), !, V1 is V0 * 100 + R, hand_value(XS, V1, V).

map_rank2(0'J,  1).
map_rank2(C, N) :- map_rank(C, N).

hand_value2([], V, V).
hand_value2([X|XS], V0, V) :- map_rank2(X, R), !, V1 is V0 * 100 + R, hand_value2(XS, V1, V).

hand_combs(_, [], Hand, Hand).
hand_combs(_, `JJJJJ`, [], `AAAAA`).
hand_combs(V, [0'J|CS], Hand, MaxHand) :-
    maplist(append_to_hand(Hand), `23456789TQKA`, Hands0),
    maplist(hand_combs(V, CS), Hands0, Hands1),
    max_member(cmp_hands(V), MaxHand, Hands1).
hand_combs(V, [C|CS], Hand, MaxHand) :- append(Hand, [C], Hand1), hand_combs(V, CS, Hand1, MaxHand).

cmp_hands(V, A, B) :- hand_val(V, A, VA), hand_val(V, B, VB), VA =< VB.

append_to_hand(H0, C, H) :- append(H0, [C], H).
