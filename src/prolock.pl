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
    flushVaultToDisk(Vault, Key), 
    performVaultActions(Vault, Key, "").


% Existing user/returning user workflow
% A user must already exist
% TODO: test this - Aziz
existingUserWorkflow :- 
    showWelcomeBack,
    askForKey(Key),
    openVault(Vault, Key),
    performVaultActions(Vault, Key, "").


% Takes inputs from user and performs actions on the given vault
% TODO: test this - Aziz
performVaultActions(Vault, Key, ValueFromPrevCmd) :- 
    showLogo,
    printOutputFromCommand(ValueFromPrevCmd),
    getNextVaultAction(Command),
    perform(Command,Vault,Key,NewVault, Output),
    (
        (isExitCommand(Command), !);  % vault is already locked to just exit
        (
            not(isExitCommand(Command)),
            performVaultActions(NewVault, Key, Output)
        )
    ).


% True when the user has added a username/password
% NewVault is the updated Vault after the action has been done
% TODO: test this
perform(add, Vault, Key, NewVault, Output) :- 
    writeln("\nEnter details for the new credentials..."),
    nl,
    getDomain(Domain),
    getUsername(Username), 
    getPassword(Password),
    addToVault(record(Domain,Username,Password),Vault,NewVault),
    concatList(["Success! Added <", Username, ", ", Password, ">", " to ", Domain], Output).


% True when the user has deleted a record
% NewVault is the updated Vault after the action has been done
% TODO: test this
perform(del, Vault, Key, NewVault, Output) :- 
    Actions = [
        command("1", "Delete username/password", delCred),
        command("2", "Delete entire domain", delDomain)
    ],
    writeln("What would you like to delete?"),
    getChoice(Choice, Actions),
    perform(Choice, Vault, Key, NewVault, Output).


% True when the user has deleted a credential
% NewVault is the updated Vault after the action has been done
% TODO: test this
perform(delCred, Vault, Key, NewVault, Output) :- 
    writeln("\nEnter details for credential to delete..."),
    getDomain(Domain),
    getUsername(Username),
    getFromVault(record(Domain,Username,_), Vault,Results),
    (
        (
            Results = empty,
            concatList(["No such record: <", Username, ">", " in ", Domain], Output), 
            NewVault = Vault, !
        );
        (
            not(Results = empty),
            concatList(["Success! Deleted <", Username, ">", " from ", Domain], Output),
            deleteFromVault(record(Domain,Username,_), Vault, NewVault), !
        )
    ).


% True when the user has deleted an entire domain
% NewVault is the updated Vault after the action has been done
% TODO: test this
perform(delDomain, Vault, Key, NewVault, Output) :-  
    writeln("\nEnter details for domain to delete..."),
    getDomain(Domain),
    getFromVault(record(Domain,_,_), Vault,Results),
    (
        (
            Results = empty,
            concatList(["No such domain:", Domain], Output), 
            NewVault = Vault, !
        );
        (
            not(Results = empty),
            concatList(["Success! Deleted ", Domain], Output),
            deleteFromVault(record(Domain,_,_), Vault, NewVault), !
        )
    ).


% True when the user has looked-up a username/password
% NewVault is the updated Vault after the action has been done
% TODO: test this
perform(lookup, Vault, Key, Vault, Output) :- 
    writeln("\nEnter details for search..."),
    Actions = [
        command("1", "Entire vault", lookupVault),
        command("2", "Domain in vault", lookupDomain)
    ],
    writeln("\nSelect a search scope..."),
    getChoice(Choice1, Actions),
    (
        (
            Choice1 = lookupVault, !
        );
        (
            Choice1 = lookupDomain, 
            getDomain(Domain),
            DomainActions = [
                command("1", "Show entire domain", lookupDomain),
                command("2", "Look for record in domain", lookupCred)
            ],
            writeln("\nSelect a search scope..."),
            getChoice(Choice2, DomainActions),
            (
                (
                    Choice2 = lookupCred,
                    getUsername(Username), !
                );
                (
                    Choice2 = lookupDomain, !
                )
            ), !
        )
    ),
    getFromVault(record(Domain,Username,_), Vault,Results),
    prettyStringResults(Results, ResultString),
    concatList([
        "Results from Vault:\n",
        ResultString
    ], Output).


% True when the exit action has been performed
% NewVault is the updated Vault after the action has been done
% TODO: test this
perform(exit, Vault, Key, NewVault, _) :- 
    lockVault(Key, Vault),
    showLogo,
    nl,
    writeln("Locking the vault. Please wait..."),
    nl,
    showLockingAnimation(30),
    sayBye.


% Entry-point for Prolock.
% Either initiate newUserWorkflow or existingUserWorkflow
prolock :- userExists, existingUserWorkflow, !.
prolock :- not(userExists), newUserWorkflow.

:- initialization(prolock, main).
