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
openVault(Vault, Key) :- 
    readNonceAndTag(Nonce,Tag),
    readVault(Key,Nonce,Tag,Vault).

readVault(Key,Nonce,Tag,Vault) :-
    vaultFile(VaultFile),
    readData(EncryptedVaultData, VaultFile),
    decrypt(StringVaultData, Key, Nonce, Tag, EncryptedVaultData),
    stringToDict(Vault, StringVaultData).

readNonceAndTag(Nonce,Tag) :- 
    nonceFile(NonceFile),
    readHexBytes(Nonce, NonceFile),
    tagFile(TagFile),
    readHexBytes(Tag, TagFile).


% True if `Vault` is encrypted and saved to disk using the given password
% `Vault` is a key-value dictionary (see dict.pl)
lockVault(Key, Vault) :- 
    writeVault(Key,Vault, Nonce, Tag),
    writeKey(Key),
    writeNonceAndTag(Nonce,Tag).

writeVault(Key, Vault, Nonce, Tag) :-
    vaultFile(VaultFile),
    dictToString(Vault, VaultString),
    encrypt(VaultString, Key, Nonce, Tag, EncryptedVaultString),
    writeData(EncryptedVaultString, VaultFile).

writeKey(Key) :-
    keyHashFile(KeyFile),
    hash(Key, KeyHash),
    writeData(KeyHash,KeyFile).

writeNonceAndTag(Nonce,Tag) :- 
    nonceFile(NonceFile),
    writeHexBytes(Nonce, NonceFile),
    tagFile(TagFile),
    writeHexBytes(Tag, TagFile).


% True if the given Vault has been flushed to disk with the given Key
% Nonce and Tag are generated
% TODO: implement this
flushVaultToDisk(Vault, Key) :- 
    % TODO: lock the vault with the password (vault:lockVault)
    % TODO: reopen vault (vault:openVault)
    notImplemented("vault -> flushVaultToDisk").


% Vault structure:
%   <Domain1>: (eg. www.amazon.com)
%       <Username1>:<Password1> (eg. admin:password123)
%       <Username2>:<Password2>
%   <Domain2>:
%       <Username1>:<Password1>
%       ...

% True if NewVault is Vault with the given record added
% TODO: implement this
addToVault(record(Domain,Username,Password),Vault,NewVault) :- notImplemented("vault -> addToVault").


% True if NewVault is Vault with the given record removed
% Domain must be bound. If Username is not bound, then all records for domain are removed.
% If Username is bound, only the record for the given username is removed fromd domain.
% TODO: implement this
deleteFromVault(record(Domain,Username,_),Vault,NewVault) :- notImplemented("vault -> deleteFromVault").


% True if Results is a dictionary of records in Vault that match given record
% If Domain is not bound, then Results = Vault
% If Domain is bound, but Username is not, then Results is all results for the domain
% If Domain and Username is bound, then Results is the record for that username in the domain
% TODO: implement this
getFromVault(record(Domain,Username,_),Vault,Results) :- notImplemented("vault -> getFromVault").