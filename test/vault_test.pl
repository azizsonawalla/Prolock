:- begin_tests(vault).
:- [src/vault].
:- [src/disk].

% To run the tests:
% ?- run_tests.

test(vaultExists) :- 
    createMockVault,
    vaultExists,
    deleteMockVault,
    not(vaultExists).


% Helper predicate that creates dummy vault files
createMockVault :-
    vaultFile(VaultFile), writeData("mock vault data", VaultFile),
    keyHashFile(KeyFile), writeData("mock key hash", KeyFile),
    nonceFile(NonceFile), writeData("mock nonce", NonceFile),
    tagFile(TagFile), writeData("mock tag", TagFile).


% Helper predicate that deletes the dummy vault files
deleteMockVault :-
    vaultFile(VaultFile), delete(VaultFile),
    keyHashFile(KeyFile), delete(KeyFile),
    nonceFile(NonceFile), delete(NonceFile),
    tagFile(TagFile), delete(TagFile).

:- end_tests(vault).
