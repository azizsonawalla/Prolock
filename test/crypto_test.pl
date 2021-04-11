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


test(hash) :-
    hash("this is an input", "6617fd6198edd307a4f736c5d874cbf174ef509ca79d1506512dcffb068ee506"),
    hash("this has $peci@l chars!", "c456573db0892500720d277d618005dda2a0d245d38c9fe40cbe14e72df69a63"),
    % Check it produces the same hash every time
    hash("sample input", Hash),
    hash("sample input", Hash).


:- end_tests(crypto).

:- initialization(run_tests, main).
