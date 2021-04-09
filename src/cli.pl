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
    getPromptReply("Vault password", PasswordAttempt),
    (
        (
            isCorrectPassword(PasswordAttempt),
            nl,
            writeln("Vault unlocked!"),
            Key = PasswordAttempt, !
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
    getPromptReply("New vault password", FirstPasswordEntry),
    getPromptReply("Re-enter password", SecondPasswordEntry),
    (
        (
            FirstPasswordEntry = SecondPasswordEntry, 
            writeln("New vault created."),
            Key = SecondPasswordEntry, !                        
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
    concatList(["\t", Number, ": ", Description, "."], String),
    writeln(String).


% True if NextCommand is the next command from the user
% TODO: test this - Aziz
getNextCommand(NextCommand) :- 
    nl,
    writeln("What would you like to do?"),
    nl,
    commands(CommandList),
    forall(
        member(Command,CommandList),
        prettyPrint(Command)
    ),
    nl,
    prompt(_, '> Enter number: '),
    readln([CommandNumberAtom|Rest]),
    term_string(CommandNumberAtom, CommandNumber),    
    writeln(CommandNumber),
    % TODO: validate input
    findall(
        Atom,
        member(command(CommandNumber,_,Atom),CommandList),
        [NextCommand|Rest]
    ).


getPromptReply(Prompt,Reply) :-
    concatList(["> ", Prompt, ": "], FormattedPrompt),
    prompt(_, FormattedPrompt),
    readln(ReplyAtomsList),
    atomic_list_concat(ReplyAtomsList, ReplyAtom),
    atom_string(ReplyAtom, Reply).


% True if String is the concatenation of all strings in List
concatList(List,String) :-
    reverse(List, Reverse),
    foldl(concat, Reverse, "", String).    


% True if the given command is the exit command
isExitCommand(exit).


% True if the user was shown the given dictionary
% TODO: Implement this
prettyPrintDict(Dict) :- notImplemented.