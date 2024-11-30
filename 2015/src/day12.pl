:- ensure_loaded(utils).

main :-
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(111754, 65402).

solve(P1, P2) :-
    read_file_to_codes('../inputs/day12.txt', Codes, []),
    all_nums(Codes, Nums), sum_list(Nums, P1),
    no_red(Codes, Codes2, [], _), all_nums(Codes2, Nums2), sum_list(Nums2, P2).

no_red([], [], [], false).
no_red([0'{|CS], XS, Rest, Red) :-
    no_red(CS, XS00, Rest0, Red0),
    (Red0 = true -> XS0 = []; XS0 = XS00),
    (Rest0 = [0'}]
        -> Rest = Rest0, XS = XS0, Red = false
        ; no_red(Rest0, XS1, Rest1, Red), append(XS0, XS1, XS), Rest = Rest1
    ).
no_red([0'}|CS], [], CS, false).
no_red([0':,0'",0'r,0'e,0'd,0'"|CS], XS, Rest, true) :- no_red(CS, XS, Rest, _).
no_red([C|CS], [C|XS], Rest, Red) :- no_red(CS, XS, Rest, Red).
