:- module(cli).
:- [src/errors].
:- [src/vault].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for CLI interactions with user %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% True if the user was shown the Prolock logo
showLogo :- 
    write('\e[H\e[2J'), % clear screen
    writeln("█████╗   ██████╗   ██████╗  ██╗       ██████╗   ██████╗ ██╗  ██╗"),
    writeln("██╔══██╗ ██╔══██╗ ██╔═══██╗ ██║      ██╔═══██╗ ██╔════╝ ██║ ██╔╝"),
    writeln("██████╔╝ ██████╔╝ ██║   ██║ ██║      ██║   ██║ ██║      █████╔╝ "),
    writeln("██╔═══╝  ██╔══██╗ ██║   ██║ ██║      ██║   ██║ ██║      ██╔═██╗ "),
    writeln("██║      ██║  ██║ ╚██████╔╝ ███████╗ ╚██████╔╝ ╚██████╗ ██║  ██╗"),
    writeln("╚═╝      ╚═╝  ╚═╝  ╚═════╝  ╚══════╝  ╚═════╝   ╚═════╝ ╚═╝  ╚═╝"),
    nl.


% True if the user was shown the first-time welcome message
showFirstTimeWelcome :- 
    showLogo,
    writeln("Welcome new user!"),
    writeln("Create a new vault to get started."),
    nl.


% True if the user was shown the returning user welcome message
showWelcomeBack :- 
    showLogo,
    writeln("Welcome back!"),
    nl.


% True if the user was shown the goodbye message
sayBye :- 
    writeln("Vault has been locked."),
    writeln("Thank you and goodbye!"). 


% True if Key is the verified password entered by the user for an existing Vault
% Should be able to read special chars!
% TODO: test when readData has been implemented - Aziz
askForKey(Key) :- 
    prompt(_, '> Vault password: '),
    readln(PasswordAttempt),
    (
        (
            isCorrectPassword(PasswordAttempt),
            nl,
            writeln("Vault unlocked!"),
            atomic_list_concat(PasswordAttempt, KeyAtom),
            atom_string(KeyAtom, Key), !
        ),
        (
            not(isCorrectPassword(PasswordAttempt)),
            nl,
            writeln("Invalid input - password is incorrect. Try again."),
            askForKey(Key), !
        )
    ).


% True if Key is a new password created by the user
% Should be able to read special chars!
% TODO: test when readData has been implemented - Aziz
askForNewKey(Key) :- 
    nl,
    writeln("Set new password for vault."),
    writeln("You may use letters, numbers, and ~!@#$^&*."),
    nl,
    prompt(_, '> New vault password: '),
    readln(FirstPasswordEntry),
    prompt(_, '> Re-enter password: '),
    readln(SecondPasswordEntry),
    (
        (
            FirstPasswordEntry = SecondPasswordEntry, 
            writeln("New vault created."),
            atomic_list_concat(SecondPasswordEntry, KeyAtom),
            atom_string(KeyAtom, Key), !                        
        );
        (
            not(FirstPasswordEntry = SecondPasswordEntry),
            writeln("Invalid input - Passwords do not match. Try again."),
            askForNewKey(Key), !
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