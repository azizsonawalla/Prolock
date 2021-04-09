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
% TODO: test this - Aziz
newUserWorkflow :- 
    showFirstTimeWelcome,
    askForNewKey(NewKey),
    newVault(NewVault),
    flushVaultToDisk(Vault, NewKey),
    performVaultActions(Vault, NewKey).


% Existing user/returning user workflow
% A user must already exist
% TODO: test this - Aziz
existingUserWorkflow :- 
    showWelcomeBack,
    askForKey(Key),
    openVault(Vault, Key),
    performVaultActions(Vault, Key).


% Takes inputs from user and performs actions on the given vault
% TODO: test this - Aziz
performVaultActions(Vault, Key) :- 
    getNextCommand(Command),
    perform(Command,Vault,Key,NewVault),
    (
        isExitCommand(Command);  % vault is already locked to just exit
        (
            not(isExitCommand(Command)),
            performVaultActions(NewVault,Key)
        )
    ).


% True when the user has added a username/password
% NewVault is the updated Vault after the action has been done
% TODO: implement this
perform(add, Vault, Key, NewVault) :- 
    % Ask for domain
    % Ask for username
    % Ask for password
    % addToVault
    notImplemented("prolock -> perform").


% True when the user has deleted a username/password
% NewVault is the updated Vault after the action has been done
% TODO: implement this
perform(del, Vault, Key, NewVault) :- 
    % Ask for domain
    % Ask for username (if any)
    % Delete from vault
    notImplemented("prolock -> perform").


% True when the user has looked-up a username/password
% NewVault is the updated Vault after the action has been done
% TODO: implement this
perform(lookup, Vault, Key, NewVault) :- 
    % Ask for domain (* = all domains)
    % Ask for usernames (* = all usernames)
    % Show results
    notImplemented("prolock -> perform").


% True when the exit action has been performed
% NewVault is the updated Vault after the action has been done
% TODO: implement this
perform(exit, Vault, Key, NewVault) :- notImplemented("prolock -> perform").


% Entry-point for Prolock.
% Either initiate newUserWorkflow or existingUserWorkflow
main :- userExists, existingUserWorkflow.
main :- newUserWorkflow.