:- module(cli).
:- [src/errors].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for CLI interactions with user %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% True if the user was shown the first-time welcome message
showFirstTimeWelcome :- 
    writeln('Welcome to Prolock'), nl.


% True if the user was shown the returning user welcome message
showWelcomeBack :- 
    writeln('Welcome back to Prolock'), nl.


% True if the user was shown the goodbye message
sayBye :- writeln('Thank you and goodbye.'), nl.


% True if Key is the verified password entered by the user for an existing Vault
% Should be able to read special chars!
askForKey(Key) :- 
    writeln('Please loggin.'),
    prompt(_, 'Password: '),
    readln(Keys),
    atomic_list_concat(Keys, Attpmt),
    (
        (
            isCorrectPassword(Key),
            writeln('Vault unlocked!')
        ),
        (
            not(isCorrectPassword(Key)),
            writeln('Invalid input - password is incorrect. Try again.'),
            askForKey(Key)
        )
    ).



% True if Key is a new password created by the user
% Should be able to read special chars!
askForNewKey(Key) :- 
    write('You may use letter, numbers, and special characters but spaces will be removed.'), nl,
    prompt(_, 'Password: '),
    readln(Keys),
    atomic_list_concat(Keys, Key1),
    prompt(_, 'Re-enter password: '),
    readln(Keys),
    atomic_list_concat(Keys, Key2),
    (
        (
            Key1 = Key2,
            writeln('New vault created.'),
            Key = Key1
        );
        (
            not(Key1 = Key2),
            writeln('Passwords do not match. Please try again.'),
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
% TODO: Implement this
getNextCommand(NextCommand) :- 
    nl,
    writeln("What would you like to do? (Enter the corresponding number)"),
    nl,
    commands(CommandList),
    forall(
        member(Command,CommandList),
        prettyPrint(Command)
    ),
    % TODO: get number input
    % TODO: lookup atom for corresponding command
    notImplemented.


% True if the given command is the exit command
isExitCommand(exit).


% True if the user was shown the given dictionary
% TODO: Implement this
showDictionary(Dict) :- notImplemented.