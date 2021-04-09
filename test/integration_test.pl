:- begin_tests(integration).
:- [src/disk].
:- [src/crypto].
:- [src/vault].

% To run the tests:
% ?- run_tests.

test(encrypt_write_read_decrypt) :-
    OriginalData = "this is a secret message",                % create a secret message
    Key = "this is a password",                               % create a password
    vaultFile(VaultFile),                                     % get the name of the vault file
    encrypt(OriginalData, Key, Nonce, Tag, Encrypted),        % encrypt the secret message
    writeData(Encrypted, VaultFile),                          % write the encrypted version to disk
    readData(EncryptedFromDisk, VaultFile),                   % read the encrypted version back from disk
    decrypt(Decrypted, Key, Nonce, Tag, EncryptedFromDisk),   % decrypt the version read from disk
    Decrypted = OriginalData.                                 % check that the decrypted version is the same as the original

:- end_tests(integration).

:- initialization(run_tests, main).
