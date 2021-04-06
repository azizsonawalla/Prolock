:- module(prolock_vault, [vaultExists/0, isCorrectPassword/1, openVault/2, lockVault/2, vaultFile/1, keyHashFile/1, nonceFile/1, tagFile/1]).
:- [src/errors].
:- [src/disk].

%% An encrypted vault %%


% The filename for storing encrypted vault data
vaultFile("vault.txt").


% The filename for storing hashed key
keyHashFile("keyHash.txt").


% The filename for storing the Nonce
nonceFile("nonce.txt").


% The filename for storing the Tag
tagFile("tag.txt").


% True if there is vault data saved to disk (i.e. a vault exists on disk)
% TODO: test this - Aziz
vaultExists :-
    vaultFile(VaultFile), exists(VaultFile),  % vault data file exists
    keyHashFile(KeyFile), exists(KeyFile),    % key hash file exists
    nonceFile(NonceFile), exists(NonceFile),  % nonce file exists
    tagFile(TagFile), exists(TagFile).        % tag file exists


% True if the given values are the correct password for the vault
% Vault requires all three values (key, nonce, tag) to encrypt/decrypt
% TODO: implement this (1 hour) - Aziz
isCorrectPassword(password(Key, Nonce, Tag)) :- 
    % TODO: Read password hash stored on disk (see disk.pl)
    % TODO: Hash password(Key, Nonce, Tag)
    % TODO: return true if the two hashes are equal
    notImplemented.


% `Vault` is the vault from disk, decrypted using the given password
% `Vault` is a key-value dictionary (see dict.pl)
% Assumes the password is correct (use isCorrectPassword first)
% TODO: implement this (5 hour)
openVault(password(Key, Nonce, Tag), Vault) :- 
    % TODO: read encrypted data (as string) from disk (see disk.pl)
    % TODO: decrypt data using `Password`
    % TODO: build key-value dictionary from data
    notImplemented.


% True if `Vault` is encrypted and saved to disk using the given password
% `Vault` is a key-value dictionary (see dict.pl)
% TODO: implement this (5 hour)
lockVault(password(Key, Nonce, Tag), Vault) :- 
    % TODO: build string from key-value dictionary `Vault`
    % TODO: encrypt string and store it in disk (see disk.pl)
    % TODO: hash the password(Key, Nonce, Tag) and store it to disk
    notImplemented.