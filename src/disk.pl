:- module(prolock_disk, [writeData/2, readData/2]).

%% Disk Related Operations %%

% Write the given string data to disk. Overwrites any other data in the file.
% writeData(Data, Filename) is true if Data is written to the file called Filename
% TODO: implement this (2 hour)
writeData(Data, Filename) :-
    open(Filename, write, Out),
    % TODO: write the string `Data` to file.
    % TODO: test it works with special chars (encrypted text can contain all unicode chars). Might need to conver to bytes first.
    close(Out).


% Read data from the given file
% readData(Data, Filename) is true if Data is the string data read from the file called Filename
% TODO: implement this (3 hour)
readData(Data, Filename) :-
    open(Filename, read, Out),
    % TODO: read data from file as string
    % TODO: test it works with special chars (encrypted text can contain all unicode chars). Might need to write as bytes, and convert back on read.
    close(Out).
