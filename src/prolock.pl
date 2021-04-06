:- [src/errors].
:- [src/vault].

% True if a user already exists
userExists :- vaultExists.

% New user workflow
% No user exists already.
% TODO: implement this (1 hour)
newUserWorkflow :- 
    not(userExists),
    writeln("Welcome new user!"),
    % TODO: ask for password to create new vault
    % TODO: create new vault with password
    % TODO: perform vault actions (add, get, remove, exit)
    notImplemented.

% Existing user/returning user workflow
% A user must already exist
% TODO: implement this (1 hour)
existingUserWorkflow :- 
    userExists, 
    writeln("Welcome back!"),
    % TODO: ask for password
    % TODO: use password to unlock vault
    % TODO: perform vault actions (add, get, remove, exit)
    notImplemented.


% Takes inputs from user and performs actions on the given vault
% TODO: implement this (3 hours)
performVaultActions(Vault) :- 
    % TODO: Get command from user
    % TODO: If command was to exit, lock vault and exit.
    % TODO: Otherwise, perform command on given Vault (and generate new vault if required)
    % TODO: Recursively call performVaultActions(NewVault)
    notImplemented.


% Entry-point for Prolock.
% Either initiate newUserWorkflow or existingUserWorkflow
main :- newUserWorkflow ; existingUserWorkflow.