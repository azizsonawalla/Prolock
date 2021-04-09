:- module(prolock_disk, [writeData/2, readData/2, exists/1, delete/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Disk Related Operations %
%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Write the given string data to disk. Overwrites any other data in the file.
% writeData(Data, Filename) is true if Data is written to the file called Filename
% TODO: implement this (2 hour) - Matthew
writeData(Data, Filename) :-
    open(Filename, write, Out),
    % TODO: write the string `Data` to file.
    % TODO: test it works with special chars (encrypted text can contain all unicode chars). 
    %       Might need to conver to bytes first.
    write(Out, Data), nl, 
    close(Out).


% Read data from the given file
% readData(Data, Filename) is true if Data is the string data read from the file called Filename
% TODO: implement this (3 hour) - Matthew
readData(Data, Filename) :-
    open(Filename, read, Out),
    % TODO: read data from file as string
    % TODO: test it works with special chars (encrypted text can contain all unicode chars). 
    %       Might need to write as bytes, and convert back on read.
    get_char(Out, Char1),
    process_stream(Char1, Out, ''),
    close(Out).


process_stream(end_of_file, _, String) :- 
    write(String), nl, !.
process_stream(Char, Out, String) :-
    atom_concat(String, Char, X),
    get_char(Out, Char2),
    process_stream(Char2, Out, X).


% True if a file with the given name exists
exists(Filename) :- exists_file(Filename).


% True if the file with the given name has been deleted
delete(Filename) :- delete_file(Filename).
