:- ensure_loaded(utils).

main :- 
    solve(P1, P2), !,
    writeln(P1), writeln(P2),
    solutions(P1, P2).

solutions(255, 55).

solve(P1, P2) :-
    read_file_to_lines('../inputs/day05.txt', Lines),
    include(is_nice1, Lines, Nice1), length(Nice1, P1),
    include(is_nice2, Lines, Nice2), length(Nice2, P2).

is_nice1(Str) :-
    string_chars(Str, Chars),
    include(is_vowel, Chars, Vowels), length(Vowels, X), X >= 3,
    contains_double(Chars),
    \+ sub_string(Str, _, 2, _, "ab"), \+ sub_string(Str, _, 2, _, "cd"),
    \+ sub_string(Str, _, 2, _, "pq"), \+ sub_string(Str, _, 2, _, "xy").

is_vowel('a').
is_vowel('e').
is_vowel('i').
is_vowel('o').
is_vowel('u').

contains_double([C,C|_]).
contains_double([_|CS]) :- contains_double(CS).

is_nice2(Str) :-
    sub_string(Str, B1, 2, _, Pair), sub_string(Str, B2, 2, _, Pair), B2 > B1 + 1,
    sub_string(Str, _, 3, _, Tree), string_chars(Tree, [A, _, A]).
