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

% adding to an existing domain name, name did not exist
test(addToVault1) :- 
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    addToVault(record("www.google.com","dylan","d123g"),Vault,NewVault),
    NewVault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", dict("dylan", "d123g", empty)))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))).

% adding to an existing domain name, name existed 1
test(addToVault2) :- 
    Vault = dict(
    "www.google.com", dict("charles", "c123g", empty), empty),
    addToVault(record("www.google.com","charles","newpass"), Vault, NewVault),
    NewVault = dict(
    "www.google.com", dict("charles", "newpass", empty), empty).

% adding to an existing domain name, name existed 2
test(addToVault3) :- 
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    addToVault(record("www.samepass.com","amy","newpass"),Vault,NewVault),
    NewVault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "newpass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))).


% adding to a nonexisting domain name, should create new domain name dictionary
test(addToVault4) :- 
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    NewVault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))),
    dict(
    "www.newdomain.com", dict("charles", "c123n", empty), empty
    )))),
    addToVault(record("www.newdomain.com","charles","c123n"),Vault,NewVault).

% adding to a nonexisting domain name, add two different records consecutively 
test(addToVault5) :- 
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    addToVault(record("www.newdomain.com","charles","c123n"),Vault,NewVault),
    NewVault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))),
    dict(
    "www.newdomain.com", dict("charles", "c123n", empty), empty
    )))),
    addToVault(record("www.newdomain.com","amy","a123n"),NewVault,NewNewVault),
    NewNewVault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))),
    dict(
    "www.newdomain.com", dict("charles", "c123n", dict("amy", "a123n", empty)), empty
    )))).

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
    deleteFromVault(record("www.newdomain.com", Variable, "c123n"), Vault, NewVault),
    NewVault = empty.

% simple case uesrname bounded
test(deleteFromVault1) :- 
    Vault = dict("www.newdomain.com", dict("charles", "c123n", empty), empty),
    deleteFromVault(record("www.newdomain.com", "charles", "d123c"), Vault, empty).

% simple case uesrname NOT bounded
test(deleteFromVault1_unbound) :- 
    Vault = dict("www.newdomain.com", dict("charles", "c123n", dict("amy", "n123a",empty)), empty),
    deleteFromVault(record("www.newdomain.com", Variable, "c123n"), Vault, NewVault),
    NewVault = empty.

% delete and insert
test(deleteFromVaultAndAdd) :- 
    Vault = dict("www.newdomain.com", dict("charles", "c123n", empty), empty),
    deleteFromVault(record("www.newdomain.com", Variable, "c123n"), Vault, NewVault),
    addToVault(record("www.reddit.com", "amy", "r123a"), NewVault, NewNewVault),
    NewNewVault = dict("www.reddit.com", dict("amy", "r123a", empty), empty).

% delete from an existing domain name, uesrname bounded, name did not exist. So no changes
test(deleteFromVault2) :- 
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    deleteFromVault(record("www.google.com", "dylan", "d123g"), Vault, Vault).

% delete from an existing domain name,  uesrname bounded, name exist, but password are different. Still remove the record
test(deleteFromVault3) :- 
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    NewVault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", empty)),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    deleteFromVault(record("www.google.com", "amy", "wrongpass"), Vault, NewVault).

% delete from an existing domain name, uesrname bounded, name exist, password is correct. Also remove the record
test(deleteFromVault4) :- 
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    NewVault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", empty)),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    deleteFromVault(record("www.google.com", "amy", "a123g"), Vault, NewVault).


% delete from an existing domain name, uesrname NOT bounded. Delete all records for that domain
test(deleteFromVault5) :- 
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    NewVault = dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    )),
    deleteFromVault(record("www.google.com", Variable, "a123g"), Vault, NewVault).

% delete from an existing domain name, uesrname NOT bounded, password not bounded. Delete all records for that domain
test(deleteFromVault6) :- 
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    NewVault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    )),
    deleteFromVault(record("www.samepass.com", _, _), Vault, NewVault).

% delete from a non-existing domain name, uesrname bounded. No changes.
% TODO this test case should pass but Prolog is not allowing; we should explicitly test for this condition in the terminal
test(deleteFromVault7) :- 
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    deleteFromVault(record("www.doesnotexist.com", "charles", "123"), Vault, Res),
    Vault = Res, % this test case fails because this line is false, but the values are actually the same!
    writeln(""),
    writeln(Vault),
    writeln(Res).
 
% delete from a non-existing domain name, uesrname NOT bounded. No changes.
test(deleteFromVault8) :- 
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    NewVault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    deleteFromVault(record("www.doesnotexist.com", Variable, "123"), Vault, NewVault).


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
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    getFromVault(record("www.difpeople.com", "lily", "l123u"), Vault, Results),
    Results = record("www.difpeople.com", "lily", "l123u").

test(getFromVault_all_bound_invalid) :-
    writeln("getFromVault_all_bound_invalid"),
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    getFromVault(record("www.invaild.com", "invalidName", "invalidPas"), Vault, Results),
    % Results = empty.
    writeln(Results).

test(getFromVault_all_bound_valid2) :- 
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    getFromVault(record("www.difpeople.com", "lily", "wrongpass"), Vault, Results),
    Results = record("www.difpeople.com", "lily", "l123u").

test(getFromVault_domain_unbound_valid) :-
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    getFromVault(record(_, "something", "anything"), Vault, Results),
    Results = Vault.

% there's no possible case for "domain unbound invalid"

test(getFromVault_username_unbound_valid) :-
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    getFromVault(record("www.difpeople.com", _, "anything"), Vault, Results),
    Results = dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))).

test(getFromVault_username_unbound_invalid) :-
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    getFromVault(record("www.invalid.com", _, "c123g"), Vault, Results),
    Results = empty.

test(testAllAddRemoveGet) :-
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))),
    addToVault(record("www.newdomain.com", "charles", "n123c"), Vault, Vault1),
    deleteFromVault(record("www.samepass.com", _, "abc"), Vault1, Vault2),
    addToVault(record("www.google.com", "gonzales", "g123g"), Vault2, Vault3),
    deleteFromVault(record("www.google.com", "zack", "z123g"), Vault3, Vault4),
    addToVault(record("www.newdomain.com", "larry", "n123l"), Vault4, Vault5),
    Vault6 = dict(
    "www.google.com", dict("charles", "c123g", dict("amy", "a123g", dict("gonzales", "g123g", empty))),
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))),
    dict(
    "www.newdomain.com", dict("charles", "n123c", dict("larry", "n123l", empty)), empty
    ))),
    Vault5 = Vault6,
    getFromVault(record(_, _, _), Vault5, Vault5),
    getFromVault(record("www.google.com", _, "abc"), Vault5, Result1),
    Result1 = dict("charles", "c123g", dict("amy", "a123g", dict("gonzales", "g123g", empty))).
    getFromVault(record("www.google.com", "charles", _), Vault5, record("www.google.com", "charles", "c123g")),
    getFromVault(record("invalid", _, _), Vault5, empty).
    getFromVault(record("www.google.com", "invalid", _), empty),
    getFromVault(record("www.samepass.com", _, _), Vault5, empty),
    getFromVault(record("www.newdomain.com", _, _), Vault5, Result2),
    Result2 = dict("charles", "n123c", dict("larry", "n123l", empty)).
    


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
    Vault = dict(
    "www.google.com", dict("charles", "c123g", dict("zack", "z123g", dict("amy", "a123g", empty))),
    dict(
    "www.samepass.com", dict("charles", "samepass", dict("zack", "samepass", dict("amy", "samepass", empty))), 
    dict(
    "www.difpeople.com",  dict("amy", "a123u", dict("lily", "l123u", dict("bob", "b123u", empty))), empty
    ))).

:- end_tests(vault).

:- initialization(run_tests, main).
