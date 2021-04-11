:- module(prolock_vault).
:- [src/errors].
:- [src/disk].
:- [src/crypto].
:- [src/dictionary].


%%%%%%%%%%%%%%%%%%%%%%
% An encrypted vault %
%%%%%%%%%%%%%%%%%%%%%%


% The filename for storing encrypted vault data
vaultFile("vault.txt").


% The filename for storing hashed key
keyHashFile("keyHash.txt").


% The filename for storing the Nonce
nonceFile("nonce.txt").


% The filename for storing the Tag
tagFile("tag.txt").


% True if Vault is a fresh vault
newVault(Vault) :- Vault = empty.


% True if there is vault data saved to disk (i.e. a vault exists on disk)
vaultExists :-
    vaultFile(VaultFile), exists(VaultFile),  % vault data file exists
    keyHashFile(KeyFile), exists(KeyFile),    % key hash file exists
    nonceFile(NonceFile), exists(NonceFile),  % nonce file exists
    tagFile(TagFile), exists(TagFile).        % tag file exists


% True if the given values are the correct key for the vault
isCorrectPassword(GivenKey) :- 
    keyHashFile(KeyFile),
    readData(StoredKeyHash, KeyFile),
    hash(GivenKey, GivenKeyHash),
    GivenKeyHash = StoredKeyHash.


% `Vault` is the vault from disk, decrypted using the given password
% `Vault` is a key-value dictionary (see dict.pl)
% Assumes the password is correct (use isCorrectPassword first)
% TODO: test when readData has been implemented - Aziz
openVault(Vault, Key) :- 
    nonceFile(NonceFile),
    readData(Nonce, NonceFile),
    tagFile(TagFile),
    readData(Tag, TagFile),
    vaultFile(VaultFile),
    readData(EncryptedVaultData, VaultFile),
    decrypt(StringVaultData, Key, Nonce, Tag, EncryptedVaultData),
    stringToDict(Vault, StringVaultData).


% True if `Vault` is encrypted and saved to disk using the given password
% `Vault` is a key-value dictionary (see dict.pl)
% TODO: implement this (3 hour)
lockVault(Key, Vault) :- 
    % TODO: build string from key-value dictionary `Vault`  (see dict.pl:dictToString)
    % TODO: encrypt string and store it in disk (see disk.pl)
    % TODO: hash the password(Key, Nonce, Tag) and store it to disk
    notImplemented.


% True if the given Vault has been flushed to disk with the given Key
% Nonce and Tag are generated
% TODO: implement this
flushVaultToDisk(Vault, Key) :- 
    % TODO: lock the vault with the password (vault:lockVault)
    % TODO: reopen vault (vault:openVault)
    notImplemented.


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
% TODO: implement this
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
% TODO: implement this
getFromVault(record(Domain,Username,_), Vault, Results) :- 
    (Vault = empty ->
        Results = empty
    ;(
        not(nonvar(Domain)) ->
            Results = Vault
        ;
            (not(nonvar(Username)) ->
                (value(Domain, Records, Vault) -> 
                    Results = dict(Domain, Records, empty)
                ;
                    Results = empty
                );
                (value(Domain, Records, Vault) -> 
                    value(Username, UserPass, Records),
                    Results = record(Domain, Username, UserPass)
                ;
                    Results = empty
    )))).