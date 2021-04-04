encrypt(Input, Key, Algorithm, Encrypted, IV, Tag) :-
    crypto_n_random_bytes(12, IV),
    crypto_data_encrypt(Input, Algorithm, Key, IV, Encrypted, [tag(Tag)]).

decrypt(Input, Key, Algorithm, Encrypted, IV, Tag) :-
    crypto_data_decrypt(Encrypted, Algorithm, Key, IV, Input, [tag(Tag)]).