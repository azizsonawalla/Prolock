:- [src/errors].
:- [src/vault].
:- [src/cli].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Entry-point for the Prolock program %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% True if a user already exists
userExists :- vaultExists.

% New user workflow
% No user exists already.
% TODO: implement this (1 hour)
newUserWorkflow :- 
    not(userExists),
    showFirstTimeWelcome,
    % TODO: ask for password to create new vault (cli:askForNewKey)
    % TODO: create new vault (vault:newVault)
    % TODO: flush the vault to disk (vault:flushVaultToDisk)
    % TODO: perform vault actions (prolock:performVaultActions)
    notImplemented.

% Existing user/returning user workflow
% A user must already exist
% TODO: implement this (1 hour)
existingUserWorkflow :- 
    userExists, 
    showWelcomeBack,
    % TODO: ask for password (cli:askForKey)
    % TODO: use password to unlock vault (vault:openVault)
    % TODO: perform vault actions (prolock:performVaultActions)
    notImplemented.


% Takes inputs from user and performs actions on the given vault
% TODO: implement this (1 hour)
performVaultActions(Vault, Key) :- 
    % TODO: Get next command from user (cli:getNextCommand)
    % TODO: If command was to exit, lock vault and exit. (cli:isExitCommand)
    % TODO: Otherwise, perform command on given Vault and get new vault
    % TODO: Recursively call performVaultActions(NewVault, Key)
    notImplemented.


% True when the user has added a username/password
% NewVault is the updated Vault after the action has been done
% TODO: implement this
perform(add, Vault, Key, NewVault) :- notImplemented.


% True when the user has deleted a username/password
% NewVault is the updated Vault after the action has been done
% TODO: implement this
perform(del, Vault, Key, NewVault) :- notImplemented.


% True when the user has looked-up a username/password
% NewVault is the updated Vault after the action has been done
% TODO: implement this
perform(lookup, Vault, Key, NewVault) :- notImplemented.


% True when the exit action has been performed
% NewVault is the updated Vault after the action has been done
% TODO: implement this
perform(exit, Vault, Key, NewVault) :- notImplemented.


% Entry-point for Prolock.
% Either initiate newUserWorkflow or existingUserWorkflow
main :- newUserWorkflow ; existingUserWorkflow.