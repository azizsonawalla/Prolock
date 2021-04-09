:- module(prolock_disk, [writeData/2, readData/2, exists/1, delete/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Disk Related Operations %
%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Write the given string data to disk. Overwrites any other data in the file.
% writeData(Data, Filename) is true if Data is written to the file called Filename
writeData(Data, Filename) :-
    open(Filename, write, Stream),
    write(Stream, Data), 
    close(Stream).


% Read data from the given file
% readData(Data, Filename) is true if Data is the string data read from the file called Filename
readData(Data, Filename) :-
    open(Filename, read, Stream),
    get_char(Stream, Char1),
    process_stream(Data, Char1, Stream, ''),
    close(Stream).


process_stream(Data, end_of_file, Stream, Data) :- !.
process_stream(Data, Char, Stream, String) :-
    atom_concat(String, Char, X),
    get_char(Stream, Char2),
    process_stream(Data, Char2, Stream, X).


% True if a file with the given name exists
exists(Filename) :- exists_file(Filename).


% True if the file with the given name has been deleted
delete(Filename) :- delete_file(Filename).
