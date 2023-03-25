% Takes a file stream and returns a list of all the lines
read_file_to_lines(Filename, Lines) :- open(Filename, read, Str), read_file_to_lines_h(Str, Lines), close(Str).
read_file_to_lines_h(Stream, []) :- at_end_of_stream(Stream).
read_file_to_lines_h(Stream, [X|L]) :- \+ at_end_of_stream(Stream), read_line_to_string(Stream, X), read_file_to_lines_h(Stream, L).

% Takes a file stream and returns a list of lists of all the numbers in the file per line
read_file_to_nums(Stream, []) :- at_end_of_stream(Stream).
read_file_to_nums(Stream, [X|L]) :- \+ at_end_of_stream(Stream), read_line_to_codes(Stream, Codes), all_nums(Codes, X), read_file_to_nums(Stream, L).

% Takes a string as list of character codes and returns a list of all the numbers in the string
all_nums(Codes, Nums) :- all_nums_h(Codes, -1, Nums).
all_nums_h([], -1, []).
all_nums_h([], N, [N]).
all_nums_h([C|CS], -1, NS) :- (C < 48 ; C > 57), all_nums_h(CS, -1, NS).
all_nums_h([C|CS], N, [N|NS]) :- (C < 48 ; C > 57), all_nums_h(CS, -1, NS).
all_nums_h([C|CS], -1, NS) :- N is C - 48, all_nums_h(CS, N, NS).
all_nums_h([C|CS], N0, NS) :- N is N0 * 10 + (C - 48), all_nums_h(CS, N, NS).
