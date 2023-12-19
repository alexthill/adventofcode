% Takes a file name and returns a list of all the lines in the file as strings
read_file_to_lines_string(Filename, Lines) :- open(Filename, read, Str), read_file_to_lines(read_line_to_string, Str, Lines), close(Str).

% Takes a file name and returns a list of all the lines in the file as character codes
read_file_to_lines_codes(Filename, Lines) :- open(Filename, read, Str), read_file_to_lines(read_line_to_codes, Str, Lines), close(Str).

% Takes a function to read lines and a file stream and returnes a list of the read lines
read_file_to_lines(_, Stream, []) :- at_end_of_stream(Stream).
read_file_to_lines(Fn, Stream, [X|L]) :- \+ at_end_of_stream(Stream), call(Fn, Stream, X), read_file_to_lines(Fn, Stream, L).

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

% Takes a list of equal length lists (= 2d array = grid) and folds all members together.
% The fold function gets the member value and its x-y-position in the grid starting from 1-1
fold_grid(Fn, Rows, V0, V) :- fold_grid(Fn, Rows, 1, 1, V0, V).
fold_grid(_, [], _, _, V, V).
fold_grid(Fn, [[]|CS], _, Y, V0, V) :- succ(Y, Y1), fold_grid(Fn, CS, 1, Y1, V0, V).
fold_grid(Fn, [[C|CS]|CSS], X, Y, V0, V) :- succ(X, X1), call(Fn, X-Y, V0, C, V1), fold_grid(Fn, [CS|CSS], X1, Y, V1, V).

% Calculates the dimensions of a list of equal length lists (= 2d array = grid)
grid_size([R|RS], W, H) :- length(R, W), length([R|RS], H).
