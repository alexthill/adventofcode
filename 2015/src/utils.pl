% Takes a file stream and returns a list of all the lines
read_file_to_lines(Filename, Lines) :- open(Filename, read, Str), read_file_to_lines_h(Str, Lines), close(Str).
read_file_to_lines_h(Stream, []) :- at_end_of_stream(Stream).
read_file_to_lines_h(Stream, [X|L]) :- \+ at_end_of_stream(Stream), read_line_to_string(Stream, X), read_file_to_lines_h(Stream, L).

% Takes a file stream and returns a list of lists of all the numbers in the file per line
read_file_to_nums(Stream, []) :- at_end_of_stream(Stream).
read_file_to_nums(Stream, [X|L]) :- \+ at_end_of_stream(Stream), read_line_to_codes(Stream, Codes), all_nums(Codes, X), read_file_to_nums(Stream, L).

% Takes a string as list of character codes and returns a list of all the numbers in the string
all_nums([], []).
all_nums([0'-,C|CS], NS) :- (C < 48 ; C > 57) -> all_nums(CS, NS); N is C - 48, all_nums_neg(CS, N, NS).
all_nums([C|CS], NS) :- (C < 48 ; C > 57), all_nums(CS, NS).
all_nums([C|CS], NS) :- N is C - 48, all_nums(CS, N, NS).

all_nums([], N, [N]).
all_nums([C|CS], N, [N|NS]) :- (C < 48 ; C > 57), all_nums(CS, NS).
all_nums([C|CS], N0, NS) :- N is N0 * 10 + (C - 48), all_nums(CS, N, NS).

all_nums_neg([], N0, [N]) :- N is -N0.
all_nums_neg([C|CS], N0, [N|NS]) :- (C < 48 ; C > 57), N is -N0, all_nums(CS, NS).
all_nums_neg([C|CS], N0, NS) :- N is N0 * 10 + (C - 48), all_nums_neg(CS, N, NS).
