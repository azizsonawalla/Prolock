:- module(prolock_vault, [vaultExists/0, isCorrectPassword/1, openVault/2, lockVault/2]).
:- [src/errors].

%% An encrypted vault %%


% The filename for storing encrypted vault data
vaultFile("vault.txt").


% The filename for storing hashed password
passwordHashFile("passHash.txt").


% True if there is vault data saved to disk (i.e. a vault exists on disk)
vaultExists :-
    notImplemented.


% True if the given values are the correct password for the vault
% Vault requires all three values (key, nonce, tag) to encrypt/decrypt
isCorrectPassword(password(Key, Nonce, Tag)) :- 
    % TODO: Read password hash stored on disk (see disk.pl)
    % TODO: Hash password(Key, Nonce, Tag)
    % TODO: return true if the two hashes are equal
    notImplemented.


% `Vault` is the vault from disk, decrypted using the given password
% `Vault` is a key-value dictionary (see dict.pl)
% Assumes the password is correct (use isCorrectPassword first)
openVault(password(Key, Nonce, Tag), Vault) :- 
    % TODO: read encrypted data (as string) from disk (see disk.pl)
    % TODO: decrypt data using `Password`
    % TODO: build key-value dictionary from data
    notImplemented.


% True if `Vault` is encrypted and saved to disk using the given password
% `Vault` is a key-value dictionary (see dict.pl)
lockVault(password(Key, Nonce, Tag), Vault) :- 
    % TODO: build string from key-value dictionary `Vault`
    % TODO: encrypt string and store it in disk (see disk.pl)
    % TODO: hash the password(Key, Nonce, Tag) and store it to disk
    notImplemented.