:- module(prolock_disk, [writeData/2, readData/2, exists/1, delete/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Disk Related Operations %
%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Write the given string data to disk. Overwrites any other data in the file.
% writeData(Data, Filename) is true if Data is written to the file called Filename
writeData(Data, Filename) :-
    nl,
    writeln("Original Data:"),
    writeln(Data),
    open(Filename, write, Stream),
    base64(Data,EncodedData),
    writeln("Base64 Data:"),
    writeln(EncodedData),
    write(Stream, EncodedData),
    close(Stream).


% Read data from the given file
% readData(Data, Filename) is true if Data is the string data read from the file called Filename
readData(Data, Filename) :-
    open(Filename, read, Stream),
    readWord(Stream,EncodedData),
    writeln("Encoded Data:"),
    writeln(EncodedData),
    base64(DataAtom,EncodedData),
    atom_string(DataAtom, Data),    
    writeln("Data:"),
    writeln(Data),
    close(Stream).


readWord(InStream,W) :-
    get0(InStream,Char),
    checkCharAndReadRest(Char,Chars,InStream),
    atom_chars(W,Chars).

checkCharAndReadRest(10,[],_) :- !.  % Return
checkCharAndReadRest(32,[],_) :- !.  % Space
checkCharAndReadRest(-1,[],_) :- !.  % End of Stream
checkCharAndReadRest(end_of_file,[],_) :- !.
checkCharAndReadRest(Char,[Char|Chars],InStream) :-
    get0(InStream,NextChar),
    checkCharAndReadRest(NextChar,Chars,InStream).


% True if a file with the given name exists
exists(Filename) :- exists_file(Filename).


% True if the file with the given name has been deleted
delete(Filename) :- delete_file(Filename).
