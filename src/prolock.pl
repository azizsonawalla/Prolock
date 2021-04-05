:- [errors].
:- dynamic notImplemented/0.

% True if a user already exists
% (See vault.pl -> vaultExists)
userExists :- 
    % TODO: implement this
    notImplemented.

% New user workflow
% No user exists already.
newUserWorkflow :- 
    not(userExists),
    writeln("Welcome new user!"),
    % TODO: ask for password to create new vault
    % TODO: create new vault with password
    % TODO: perform vault actions (add, get, remove, exit)
    notImplemented. % TODO: implement this

% Existing user/returning user workflow
% A user must already exist
existingUserWorkflow :- 
    userExists, 
    writeln("Welcome back!"),
    % TODO: ask for password
    % TODO: use password to unlock vault
    % TODO: perform vault actions (add, get, remove, exit)
    notImplemented.


% Takes inputs from user and performs actions on the given vault
performVaultActions(Vault) :- 
    % TODO: Get command from user
    % TODO: If command was to exit, lock vault and exit.
    % TODO: Otherwise, perform command on given Vault (and generate new vault if required)
    % TODO: Recursively call performVaultActions(NewVault)
    notImplemented.


% Entry-point for Prolock.
% Either initiate newUserWorkflow or existingUserWorkflow
main :- newUserWorkflow ; existingUserWorkflow.