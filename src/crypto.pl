:- [errors].
:- dynamic notImplemented/0.

%% Cryptography related operations %%

% Encryption algorithm to use (uncomment just one!)
algorithm("chacha20-poly1305").
% algorithm("aes-128-gcm").
% algorithm("aes-128-cbc").


% `Encrypted` is the string `Input` encrypted with `Key`, `Nonce`, `Tag`.
% `Input` and `Key` must be provided, rest is generated
encrypt(Input, Key, Nonce, Tag, Encrypted) :-
    algorithm(Algorithm),
    crypto_n_random_bytes(12, Nonce),
    crypto_data_encrypt(Input, Algorithm, Key, Nonce, Encrypted, [tag(Tag)]).


% `Decrypted` is the string `Encrypted` decrypted with `Key`, `Nonce`, `Tag`.
% `Decrypted`, `Key`, `Nonce` and `Tag` must be provided
decrypt(Decrypted, Key, Nonce, Tag, Encrypted) :-
    algorithm(Algorithm),
    crypto_data_decrypt(Encrypted, Algorithm, Key, Nonce, Decrypted, [tag(Tag)]).


% True if `Hash` is the hashed string `Input`
% Must be a deterministic hash! (i.e. same string hashed twice should produce same hash)
hash(Input, Hash) :-
    % TODO: implement this 
    notImplemented.


% swipl ./crypto.pl
% ?- exampleEncryptDecrypt("this is a secret message", "this is the password").
exampleEncryptDecrypt(Message, Key) :-
    concat("Message: ", Message, Out),
    writeln(Out),
    concat("Key: ", Key, Out2),
    writeln(Out2),
    encrypt(Message,Key,Nonce,Tag,Encrypted),
    concat("Encrypted: ", Encrypted, Out3),
    writeln(Out3),
    decrypt(Decrypted, Key, Nonce, Tag, Encrypted),
    concat("Decrypted: ", Decrypted, Out4),
    writeln(Out4).
