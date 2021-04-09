:- module(cli).
:- [src/errors].
:- [src/vault].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for CLI interactions with user %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Logo test
logo("\n===================================\nPROLOCK ENCRYPTED VAULT\n===================================\n").

% True if the user was shown the Prolock logo
showLogo :- logo(LogoText), writeln(LogoText).


% True if the user was shown the first-time welcome message
showFirstTimeWelcome :- 
    showLogo,
    writeln("Welcome new user!"),
    nl.


% True if the user was shown the returning user welcome message
showWelcomeBack :- 
    showLogo,
    writeln("Welcome back!"),
    nl.


% True if the user was shown the goodbye message
sayBye :- 
    writeln("Vault has been locked."),
    writeln("Goodbye!"). 


% True if Key is the verified password entered by the user for an existing Vault
% Should be able to read special chars!
% TODO: test when readData has been implemented - Aziz
askForKey(Key) :- 
    writeln("Enter your vault password:"),
    readln(PasswordAttempt),
    (
        (
            isCorrectPassword(PasswordAttempt),
            writeln("Vault unlocked!"),
            Key = PasswordAttempt
        ),
        (
            not(isCorrectPassword(PasswordAttempt)),
            writeln("Invalid input - password is incorrect. Try again."),
            askForKey(Key)
        )
    ).


% True if Key is a new password created by the user
% Should be able to read special chars!
% TODO: test when readData has been implemented - Aziz
askForNewKey(Key) :- 
    writeln("Create password for new vault..."),
    nl,
    writeln("Enter password:"),
    readln(FirstPasswordEntry),
    writeln("Re-enter password:"),
    readln(SecondPasswordEntry),
    (
        (
            FirstPasswordEntry = SecondPasswordEntry, 
            writeln("New vault created."),
            Key = SecondPasswordEntry
        );
        (
            not(FirstPasswordEntry = SecondPasswordEntry),
            writeln("Invalid input - Passwords do not match. Try again."),
            askForNewKey(Key)
        )
    ).


% Supported commands where each command is command(Number,Description,atom)
commands([
    command("1", "Add a new username/password entry",           add),
    command("2", "Search for a username/password in the vault", lookup),
    command("3", "Delete an entry from the vault",              del),
    command("4", "Exit (vault will be automatically locked)",   exit)
]).


% Pretty-prints the given command
prettyPrint(command(Number, Description, _)) :- 
    reverse(["\t", Number, ": ", Description, "."], Reversed),
    foldl(concat,Reversed, "", String),
    writeln(String).


% True if NextCommand is the next command from the user
% TODO: test this - Aziz
getNextCommand(NextCommand) :- 
    nl,
    writeln("What would you like to do? (Enter the corresponding number)"),
    nl,
    commands(CommandList),
    forall(
        member(Command,CommandList),
        prettyPrint(Command)
    ),
    readln([CommandNumberAtom|Rest]),
    term_string(CommandNumberAtom, CommandNumber),    
    writeln(CommandNumber),
    % TODO: validate input
    findall(
        Atom,
        member(command(CommandNumber,_,Atom),CommandList),
        [NextCommand|Rest]
    ).


% True if the given command is the exit command
isExitCommand(exit).


% True if the user was shown the given dictionary
% TODO: Implement this
prettyPrintDict(Dict) :- notImplemented.