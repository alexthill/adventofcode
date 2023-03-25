:- use_module(library(crypto)).

main :- 
    solve(1, P1, "00000"), !, solve(P1, P2, "000000"), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(282749, 9962624).

solve(Start, N, Zeros) :-
    read_file_to_string('../inputs/day04.txt', Str, []),
    between(Start, 10000000, N),
    string_concat(Str, N, In),
    crypto_data_hash(In, MD5, [algorithm(md5)]),
    sub_string(MD5, 0, _, _, Zeros).
