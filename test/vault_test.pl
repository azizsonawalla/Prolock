:- begin_tests(vault).
:- [src/vault].
:- [src/dictionary].
:- [src/disk].

% To run the tests:
% ?- run_tests.

test(vaultExists) :-
    createMockVault(
        "mock vault data", 
        "mock key hash", 
        "mock nonce", 
        "mock tag"
    ),
    vaultExists, % mock vault should exist
    deleteMockVault,
    not(vaultExists). % mock vault should not exist


test(isCorrectPassword) :-
    createMockVault(
        "mock vault data", 
        "f54496cdb5502ac213882b7c444f1c86d8f9973e3c655fb32579dbf3bab7c375",  % The SHA-256 hash of "thisIs!ASample12334Password"
        "mock nonce", 
        "mock tag"
    ),
    isCorrectPassword("thisIs!ASample12334Password"),                        % this is the correct key
    not(isCorrectPassword("wrong key")),                                     % this is the wrong key
    deleteMockVault.


test(openVault_lockVault) :- 
    Password = "this is a password123 !",
    newVault(Vault),
    insert("Key1", "Value2", Vault, UpdatedVault),
    lockVault(Password, UpdatedVault),
    openVault(ReopenedVault, Password),
    ReopenedVault = UpdatedVault.


% Helper predicate that creates dummy vault files
createMockVault(VaultData, KeyHash, Nonce, Tag) :-
    vaultFile(VaultFile), writeData(VaultData, VaultFile),
    keyHashFile(KeyFile), writeData(KeyHash, KeyFile),
    nonceFile(NonceFile), writeData(Nonce, NonceFile),
    tagFile(TagFile), writeData(Tag, TagFile).


% Helper predicate that deletes the dummy vault files
deleteMockVault :-
    vaultFile(VaultFile), delete(VaultFile),
    keyHashFile(KeyFile), delete(KeyFile),
    nonceFile(NonceFile), delete(NonceFile),
    tagFile(TagFile), delete(TagFile).

:- end_tests(vault).

:- initialization(run_tests, main).
