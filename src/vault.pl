:- module(prolock_vault).
:- [src/errors].
:- [src/disk].
:- [src/crypto].
:- [src/dictionary].


%%%%%%%%%%%%%%%%%%%%%%
% An encrypted vault %
%%%%%%%%%%%%%%%%%%%%%%


% The filename for storing encrypted vault data
vaultFile("vault").
vaultEncryptedFile("vault.enc").


% The filename for storing hashed key
keyHashFile("keyHash.txt").


% True if Vault is a fresh vault
newVault(Vault) :- Vault = empty.


% True if there is vault data saved to disk (i.e. a vault exists on disk)
vaultExists :-
    vaultEncryptedFile(VaultEncryptedFile), exists(VaultEncryptedFile),  % vault data file exists
    keyHashFile(KeyFile), exists(KeyFile).                               % key hash file exists


% True if the given values are the correct key for the vault
isCorrectPassword(GivenKey) :- 
    keyHashFile(KeyFile),
    readData(StoredKeyHash, KeyFile),
    hash(GivenKey, GivenKeyHash),
    GivenKeyHash = StoredKeyHash.


% `Vault` is the vault from disk, decrypted using the given password
% `Vault` is a key-value dictionary (see dict.pl)
% Assumes the password is correct (use isCorrectPassword first)
openVault(Vault, Key) :- 
    readVault(Key, Vault).

readVault(Key, Vault) :-
    vaultFile(VaultFile),
    vaultEncryptedFile(VaultEncryptedFile),
    decryptFile(VaultFile, Key, VaultEncryptedFile),
    readData(StringVaultData, VaultFile),
    stringToDict(Vault, StringVaultData),
    delete(VaultFile).


% True if `Vault` is encrypted and saved to disk using the given password
% `Vault` is a key-value dictionary (see dict.pl)
lockVault(Key, Vault) :- 
    writeVault(Key,Vault),
    writeKey(Key).

writeVault(Key, Vault) :-
    vaultFile(VaultFile),
    vaultEncryptedFile(VaultEncryptedFile),
    dictToString(Vault, VaultString),
    writeData(VaultString, VaultFile),
    encryptFile(VaultFile, Key, VaultEncryptedFile),
    delete(VaultFile).

writeKey(Key) :-
    keyHashFile(KeyFile),
    hash(Key, KeyHash),
    writeData(KeyHash,KeyFile).


% True if the given Vault has been flushed to disk with the given Key
% Nonce and Tag are generated
flushVaultToDisk(Vault, Key) :- 
    lockVault(Key,Vault).

% Vault structure:
%   <Domain1>: (eg. www.amazon.com)
%       <Username1>:<Password1> (eg. admin:password123)
%       <Username2>:<Password2>
%   <Domain2>:
%       <Username3>:<Password3>
%       ...
%   dict(
%       domain1, dict(username1, password1, dict(username2, password2)),
%       dict(
%       domain2, dict(username3, password3), 
%       )
%   )
%
%
% True if NewVault is Vault with the given record added
% TODO: implement this
addToVault(record(Domain, Username, Password), Vault, NewVault) :- 
    (
    hasKey(Domain, Vault) -> 
        value(Domain, Records, Vault), 
        insert(Username, Password, Records, NewRecords),
        insert(Domain, NewRecords, Vault, NewVault)
    ;   
        insert(Domain, dict(Username, Password, empty), Vault, NewVault)
    ).


% True if NewVault is Vault with the given record removed
% Domain must be bound. If Username is not bound, then all records for domain are removed.
% If Username is bound, only the record for the given username is removed fromd domain.
deleteFromVault(record(Domain,Username,_), Vault, NewVault) :-
    ( 
    newVault(Vault) ->
        NewVault = Vault
    ;   ( 
        not(value(Domain, Records, Vault)) ->
            NewVault = Vault
        ;(
            nonvar(Username) ->
                value(Domain, Records, Vault), remove(Username, Records, NewRecords),
                (
                NewRecords == empty, Vault = dict(_, _, empty) -> 
                    NewVault = empty
                ; 
                    insert(Domain, NewRecords, Vault, NewVault)
                )
            
        ;
            remove(Domain, Vault, NewVault)
        ))
    ).


% True if Results is a dictionary of records in Vault that match given record
% If Domain is not bound, then Results = Vault
% If Domain is bound, but Username is not, then Results is all results for the domain
% If Domain and Username is bound, then Results is the record for that username in the domain
getFromVault(record(_,_,_), empty, empty) :-  !.                                               % empty Vault = empty result
getFromVault(record(Domain,_,_), Vault, Vault) :- var(Domain), !.                              % unbound Domain
getFromVault(record(Domain,Username,_), Vault, dict(Domain, Records, empty)) :- 
    var(Username), value(Domain, Records, Vault), !.                                           % unbound username, domain exists
getFromVault(record(_,Username,_), _, empty) :- 
    var(Username), !.                                                                          % unbound username, domain does not exist
getFromVault(record(Domain,Username,_), Vault, dict(Domain, dict(Username, UserPass, empty), empty)) :- 
    nonvar(Username), value(Domain, Records, Vault), value(Username, UserPass, Records), !.    % bound username, record exists
getFromVault(record(_,Username,_), _, empty) :- 
    nonvar(Username), !.                                                                       % bound username, record does not exist
