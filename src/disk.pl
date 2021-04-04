%% Disk Related Operations %%
encrypt(Input, Key, Algorithm, Encrypted, IV, Tag) :-
    crypto_n_random_bytes(12, IV),
    crypto_data_encrypt(Input, Algorithm, Key, IV, Encrypted, [tag(Tag)]).

decrypt(Input, Key, Algorithm, Encrypted, IV, Tag) :-
    crypto_data_decrypt(Encrypted, Algorithm, Key, IV, Input, [tag(Tag)]).


% Write the given string data to disk. Overwrites any other data in the file.
% writeData(Data, Filename) is true if Data is written to the file called Filename
writeData(Data, Filename) :-
    open(Filename, write, Out),
    write(Out,Data),
    close(Out).


% Read data from the given file
% readData(Data, Filename) is true if Data is the data read from the file called Filename
readData(Data, Filename) :-
    open(Filename, read, Out),
    readWord(Out,Data),
    close(Out).

readWord(InStream,Chars) :-
    get0(InStream,Char),
    checkCharAndReadRest(Char,Chars,InStream).

checkCharAndReadRest(10,[],_) :- !.  % Return
checkCharAndReadRest(32,[],_) :- !.  % Space
checkCharAndReadRest(-1,[],_) :- !.  % End of Stream
checkCharAndReadRest(end_of_file,[],_) :- !.
checkCharAndReadRest(Char,[Char|Chars],InStream) :-
    get0(InStream,NextChar),
    checkCharAndReadRest(NextChar,Chars,InStream).

writeToVault(Secret, Nonce, Tag, Encrypted) :-
    encrypt(Secret, "password", 'chacha20-poly1305', Encrypted, Nonce, Tag),
    string_codes(Encrypted, CharCodes),
    writeData(CharCodes, 'vault.txt').

readFromVault(Secret, Nonce, Tag) :-
    readData(Data, 'vault.txt'),
    term_string(CharCodes, Data),
    string_codes(Encrypted, CharCodes),
    decrypt(Secret,"password", 'chacha20-poly1305',Encrypted,Nonce,Tag).