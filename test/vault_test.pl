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


%%%%%%%%%%%%%%%%%%%%%
% add to vault      %
%%%%%%%%%%%%%%%%%%%%%


testDict(6, dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty)))).
testDict(5, dict("www.difpeople.com",  TestDict6, empty)) :- testDict(6, TestDict6).
testDict(4, dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty)))).
testDict(3, dict("www.samepass.com", TestDict4, TestDict5)) :- testDict(4, TestDict4), testDict(5, TestDict5).
testDict(2, dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty)))).
testDict(1, dict("www.google.com", TestDict2, TestDict3)) :- testDict(2, TestDict2), testDict(3, TestDict3).


% adding to an existing domain name, name did not exist
test(addToVault1) :- 
    testDict(1, Vault),
    testDict(3, TestDict3),
    addToVault(record("www.google.com","dylan","d123g"),Vault,NewVault),
    NewVault = dict(
        "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", dict("dylan", "d123g", empty)))),
        TestDict3
    ).

% adding to an existing domain name, name existed 1
test(addToVault2) :- 
    Vault = dict("www.google.com", dict("charles", "c123g", empty), empty),
    addToVault(record("www.google.com","charles","newpass"), Vault, NewVault),
    NewVault = dict("www.google.com", dict("charles", "newpass", empty), empty).

% adding to an existing domain name, name existed 2
test(addToVault3) :- 
    testDict(1, Vault),
    testDict(2, TestDict2),
    testDict(5, TestDict5),
    addToVault(record("www.samepass.com","amy","newpass"),Vault,NewVault),
    NewVault = dict(
        "www.google.com", TestDict2, 
        dict(
            "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "newpass", empty))), 
            TestDict5
        )
    ).


% adding to a nonexisting domain name, should create new domain name dictionary
test(addToVault4) :- 
    testDict(1, Vault),
    testDict(2, TestDict2),
    testDict(4, TestDict4),
    testDict(6, TestDict6),
    NewVault = dict(
        "www.google.com", TestDict2,
        dict(
            "www.samepass.com", TestDict4, 
            dict(
                "www.difpeople.com",  TestDict6,
                dict(
                    "www.newdomain.com", dict("charles", "c123n", empty), empty
                )
            )
        )
    ),
    addToVault(record("www.newdomain.com","charles","c123n"),Vault,NewVault).

% adding to a nonexisting domain name, add two different records consecutively 
test(addToVault5) :- 
    testDict(1, Vault),
    testDict(2, TestDict2),
    testDict(6, TestDict6),
    testDict(4, TestDict4),
    addToVault(record("www.newdomain.com","charles","c123n"),Vault,NewVault),
    NewVault = dict(
        "www.google.com", TestDict2,
        dict(
            "www.samepass.com", TestDict4, 
            dict(
                "www.difpeople.com",  TestDict6,
                dict(
                    "www.newdomain.com", dict("charles", "c123n", empty), empty
                )
            )
        )
    ),
    addToVault(record("www.newdomain.com","amy","a123n"),NewVault,NewNewVault),
    NewNewVault = dict(
        "www.google.com", TestDict2,
        dict(
            "www.samepass.com", TestDict4, 
            dict(
                "www.difpeople.com",  TestDict6,
                dict(
                    "www.newdomain.com", dict("charles", "c123n", dict("amy", "a123n", empty)), empty
                )
            )
        )
    ).

% adding to a nonexisting domain name, to an empty vault
test(addToVault6) :- 
    Vault = empty,
    addToVault(record("www.newdomain.com","charles","c123n"),Vault,NewVault),
    NewVault = dict("www.newdomain.com", dict("charles", "c123n", empty), empty).


%%%%%%%%%%%%%%%%%%%%%
% delete from vault %
%%%%%%%%%%%%%%%%%%%%%

% empty case uesrname bounded
test(deleteFromVaultEmpty1) :- 
    Vault = empty,
    deleteFromVault(record("www.newdomain.com", "charles", "c123n"), Vault, NewVault),
    NewVault = empty.

% empty case uesrname NOT bounded
test(deleteFromVaultEmpty1_unbound) :- 
    Vault = empty,
    deleteFromVault(record("www.newdomain.com", _, "c123n"), Vault, NewVault),
    NewVault = empty.

% simple case uesrname bounded
test(deleteFromVault1) :- 
    Vault = dict("www.newdomain.com", dict("charles", "c123n", empty), empty),
    deleteFromVault(record("www.newdomain.com", "charles", "d123c"), Vault, empty).

% simple case uesrname NOT bounded
test(deleteFromVault1_unbound) :- 
    Vault = dict("www.newdomain.com", dict("charles", "c123n", dict("amy", "n123a",empty)), empty),
    deleteFromVault(record("www.newdomain.com", _, "c123n"), Vault, NewVault),
    NewVault = empty.

% delete and insert
test(deleteFromVaultAndAdd) :- 
    Vault = dict("www.newdomain.com", dict("charles", "c123n", empty), empty),
    deleteFromVault(record("www.newdomain.com", _, "c123n"), Vault, NewVault),
    addToVault(record("www.reddit.com", "amy", "r123a"), NewVault, NewNewVault),
    NewNewVault = dict("www.reddit.com", dict("amy", "r123a", empty), empty).

% delete from an existing domain name, uesrname bounded, name did not exist. So no changes
test(deleteFromVault2) :- 
    testDict(1, Vault),
    deleteFromVault(record("www.google.com", "dylan", "d123g"), Vault, Vault).

% delete from an existing domain name,  uesrname bounded, name exist, but password are different. Still remove the record
test(deleteFromVault3) :- 
    testDict(1, Vault),
    testDict(6, TestDict6),
    testDict(4, TestDict4),
    NewVault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", empty)),
    dict(
    "www.samepass.com", TestDict4, 
    dict(
    "www.difpeople.com",  TestDict6, empty
    ))),
    deleteFromVault(record("www.google.com", "amy", "wrongpass"), Vault, NewVault).

% delete from an existing domain name, uesrname bounded, name exist, password is correct. Also remove the record
test(deleteFromVault4) :- 
    testDict(1, Vault),
    testDict(6, TestDict6),
    testDict(4, TestDict4),
    NewVault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", empty)),
    dict(
    "www.samepass.com", TestDict4, 
    dict(
    "www.difpeople.com",  TestDict6, empty
    ))),
    deleteFromVault(record("www.google.com", "amy", "a123g"), Vault, NewVault).


% delete from an existing domain name, uesrname NOT bounded. Delete all records for that domain
test(deleteFromVault5) :- 
    testDict(1, Vault),
    testDict(6, TestDict6),
    testDict(4, TestDict4),
    NewVault = dict(
    "www.samepass.com", TestDict4, 
    dict(
    "www.difpeople.com",  TestDict6, empty
    )),
    deleteFromVault(record("www.google.com", _, "a123g"), Vault, NewVault).

% delete from an existing domain name, uesrname NOT bounded, password not bounded. Delete all records for that domain
test(deleteFromVault6) :- 
    testDict(1, Vault),
    testDict(2, TestDict2),
    testDict(6, TestDict6),
    NewVault = dict(
    "www.google.com", TestDict2,
    dict(
    "www.difpeople.com",  TestDict6, empty
    )),
    deleteFromVault(record("www.samepass.com", _, _), Vault, NewVault).

% delete from a non-existing domain name, uesrname bounded. No changes.
% TODO this test case should pass but Prolog is not allowing; we should explicitly test for this condition in the terminal
test(deleteFromVault7) :- 
    testDict(1, Vault),
    deleteFromVault(record("www.doesnotexist.com", "charles", "123"), Vault, Vault).
 
% delete from a non-existing domain name, uesrname NOT bounded. No changes.
test(deleteFromVault8) :- 
    testDict(1, Vault),
    testDict(2, TestDict2),
    testDict(6, TestDict6),
    testDict(4, TestDict4),
    NewVault = dict(
    "www.google.com", TestDict2,
    dict(
    "www.samepass.com", TestDict4, 
    dict(
    "www.difpeople.com",  TestDict6, empty
    ))),
    deleteFromVault(record("www.doesnotexist.com", _, "123"), Vault, NewVault).


%%%%%%%%%%%%%%%%%%%%%
% get from vault    %
%%%%%%%%%%%%%%%%%%%%%

test(getFromVault_Empty_all_bound) :-
    Vault = empty,
    getFromVault(record("www.google.com", "charles", "g123c"), Vault, empty).

test(getFromVault_Empty_domain_unbound) :-
    Vault = empty,
    getFromVault(record(_, "charles", "g123c"), Vault, empty).

test(getFromVault_Empty_domain_unbound2) :-
    Vault = empty,
    getFromVault(record(_, _, "g123c"), Vault, empty).

test(getFromVault_Empty_user_unbound) :-
    Vault = empty,
    getFromVault(record("www.google.com", _, "g123c"), Vault, empty).

test(getFromVault_all_bound_valid) :-
    testDict(2, TestDict2),
    testDict(4, TestDict4),
    testDict(6, TestDict6),
    Vault = dict(
    "www.google.com", TestDict2,
    dict(
    "www.samepass.com", TestDict4, 
    dict(
    "www.difpeople.com",  TestDict6, empty
    ))),
    getFromVault(record("www.difpeople.com", "lily", "l123u"), Vault, Results),
    Results = record("www.difpeople.com", "lily", "l123u").

test(getFromVault_all_bound_invalid) :-
    testDict(2, TestDict2),
    testDict(4, TestDict4),
    testDict(6, TestDict6),
    Vault = dict(
    "www.google.com", TestDict2,
    dict(
    "www.samepass.com", TestDict4, 
    dict(
    "www.difpeople.com",  TestDict6, empty
    ))),
    getFromVault(record("www.invaild.com", "invalidName", "invalidPas"), Vault, Results),
    Results = empty.

test(getFromVault_all_bound_valid2) :- 
    testDict(2, TestDict2),
    testDict(4, TestDict4),
    testDict(6, TestDict6),
    Vault = dict(
    "www.google.com", TestDict2,
    dict(
    "www.samepass.com", TestDict4, 
    dict(
    "www.difpeople.com",  TestDict6, empty
    ))),
    getFromVault(record("www.difpeople.com", "lily", "wrongpass"), Vault, Results),
    Results = record("www.difpeople.com", "lily", "l123u").

test(getFromVault_domain_unbound_valid) :-
    testDict(2, TestDict2),
    testDict(4, TestDict4),
    testDict(6, TestDict6),
    Vault = dict(
    "www.google.com", TestDict2,
    dict(
    "www.samepass.com", TestDict4, 
    dict(
    "www.difpeople.com",  TestDict6, empty
    ))),
    getFromVault(record(_, "something", "anything"), Vault, Results),
    Results = Vault.

% there's no possible case for "domain unbound invalid"

test(getFromVault_username_unbound_valid) :-
    testDict(2, TestDict2),
    testDict(4, TestDict4),
    testDict(6, TestDict6),
    Vault = dict(
    "www.google.com", TestDict2,
    dict(
    "www.samepass.com", TestDict4, 
    dict(
    "www.difpeople.com",  TestDict6, empty
    ))),
    getFromVault(record("www.difpeople.com", _, "anything"), Vault, Results),
    Results = dict("www.difpeople.com",  TestDict6, empty).

test(getFromVault_username_unbound_invalid) :-
    testDict(2, TestDict2),
    testDict(4, TestDict4),
    testDict(6, TestDict6),
    Vault = dict(
    "www.google.com", TestDict2,
    dict(
    "www.samepass.com", TestDict4, 
    dict(
    "www.difpeople.com",  TestDict6, empty
    ))),
    getFromVault(record("www.invalid.com", _, "c123g"), Vault, Results),
    Results = empty.

test(testAllAddRemoveGet) :-
    testDict(2, TestDict2),
    testDict(4, TestDict4),
    testDict(6, TestDict6),
    TestDict7 = dict("charles", "c123g", dict("amy", "a123g", dict("gonzales", "g123g", empty))),
    
    Vault = dict(
        "www.google.com", TestDict2,
        dict(
            "www.samepass.com", TestDict4, 
            dict(
                "www.difpeople.com", TestDict6, empty
            )
        )
    ),
    addToVault(record("www.newdomain.com", "charles", "n123c"), Vault, Vault1),
    deleteFromVault(record("www.samepass.com", _, "abc"), Vault1, Vault2),
    addToVault(record("www.google.com", "gonzales", "g123g"), Vault2, Vault3),
    deleteFromVault(record("www.google.com", "zack", "z123g"), Vault3, Vault4),
    addToVault(record("www.newdomain.com", "larry", "n123l"), Vault4, Vault5),
    
    Vault6 = dict(
        "www.google.com", TestDict7,
        dict(
            "www.difpeople.com",  TestDict6,
            dict(
                "www.newdomain.com", dict("charles", "n123c", dict("larry", "n123l", empty)), empty
            )
        )
    ),
    Vault5 = Vault6,

    getFromVault(record(_, _, _), Vault5, Vault5),
    getFromVault(record("www.google.com", _, "abc"), Vault5, Result1),
    Result1 = dict("www.google.com", TestDict7, empty),

    getFromVault(record("www.google.com", "charles", _), Vault5, record("www.google.com", "charles", "c123g")),
    getFromVault(record("invalid", _, _), Vault5, empty),
    getFromVault(record("www.google.com", "invalid", _), Vault5, empty),
    getFromVault(record("www.samepass.com", _, _), Vault5, empty),
    getFromVault(record("www.newdomain.com", _, _), Vault5, Result2),
    Result2 = dict("www.newdomain.com", dict("charles", "n123c", dict("larry", "n123l", empty)), empty).
    

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

% Helper predicate that creates a vault dictionary
createMockVaultDict(Vault) :-
    testDict(2, TestDict2),
    testDict(4, TestDict4),
    testDict(6, TestDict6),
    Vault = dict(
    "www.google.com", TestDict2,
    dict(
    "www.samepass.com", TestDict4, 
    dict(
    "www.difpeople.com",  TestDict6, empty
    ))).

:- end_tests(vault).

:- initialization(run_tests, main).
