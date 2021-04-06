:- begin_tests(crypto).
:- [src/crypto].

% To run the tests:
% ?- run_tests.

test(encrypt_decrypt) :- 
    Input = "this is a test input to encrypt",
    Password = "a password",
    encrypt(Input, Password, Nonce, Tag, Encrypted), 
    decrypt(Decrypted, Password, Nonce, Tag, Encrypted),
    Decrypted = Input.


:- end_tests(crypto).
