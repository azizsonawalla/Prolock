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
    askForNewKey(Key),
    newVault(Vault),
    % flushVaultToDisk(Vault, Key),  % TODO: causes decryption bug when enabled
    performVaultActions(Vault, Key).


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
    getNextVaultAction(Command),
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
% TODO: test this
perform(add, Vault, Key, NewVault) :- 
    writeln("Enter details for the new credentials..."),
    nl,
    getDomain(Domain),
    getUsername(Username), 
    getPassword(Password),
    addToVault(record(Domain,Username,Password),Vault,NewVault),
    concatList(["Success! Added <", Username, ", ", Password, ">", "to ", Domain], Output),
    writeln(Output).


% True when the user has deleted a record
% NewVault is the updated Vault after the action has been done
% TODO: test this
perform(del, Vault, Key, NewVault) :- 
    Actions = [
        command("1", "Delete a credential from a domain", delCred),
        command("2", "Delete an entire domain", delDomain)
    ],
    writeln("What would you like to delete?"),
    getChoice(Choice, Actions),
    perform(Choice, Vault, Key, NewVault).


% True when the user has deleted a credential
% NewVault is the updated Vault after the action has been done
% TODO: test this
perform(delCred, Vault, Key, NewVault) :- 
    writeln("Enter details for credential to delete..."),
    getDomain(Domain),
    getUsername(Username),
    deleteFromVault(record(Domain,Username,_), Vault, NewVault), % TODO: handle case where record doesn't exist
    concatList(["Success! Deleted <", Username, ">", "from ", Domain], Output),
    writeln(Output).


% True when the user has deleted an entire domain
% NewVault is the updated Vault after the action has been done
% TODO: test this
perform(delDomain, Vault, Key, NewVault) :-  
    writeln("Enter details for domain to delete..."),
    getDomain(Domain),
    deleteFromVault(record(Domain,_,_), Vault, NewVault), % TODO: handle case where domain doesn't exist
    concatList(["Success! Deleted ", Domain], Output),
    writeln(Output).


% True when the user has looked-up a username/password
% NewVault is the updated Vault after the action has been done
% TODO: test this
perform(lookup, Vault, Key, Vault) :- 
    writeln("Enter details for record to lookup..."),
    getDomain(Domain),
    Actions = [
        command("1", "Show entire domain", lookupDomain),
        command("2", "Look for record in domain", lookupCred)
    ],
    writeln("Select a search scope..."),
    getChoice(Choice, Actions),
    (
        (
            Choice = lookupCred,
            getUsername(Username), !
        );
        (Choice = lookupDomain), !
    ),
    getFromVault(record(Domain,Username,_), Vault,Results),
    prettyPrintDict(Results).


% True when the exit action has been performed
% NewVault is the updated Vault after the action has been done
% TODO: test this
perform(exit, Vault, Key, NewVault) :- 
    lockVault(Key, Vault),
    sayBye.


% Entry-point for Prolock.
% Either initiate newUserWorkflow or existingUserWorkflow
prolock :- userExists, existingUserWorkflow.
prolock :- not(userExists), newUserWorkflow.

:- initialization(prolock, main).
