:- [errors].
:- dynamic notImplemented/0.

% True if a user already exists
% Can determine the existence of a user if vault file exists
userExists :- 
    notImplemented. % TODO: implement this

% New user workflow
% No user exists already.
newUserWorkflow :- 
    not(userExists),
    writeln("Welcome new user!"),
    notImplemented. % TODO: implement this

% Existing user/returning user workflow
% A user must already exist
existingUserWorkflow :- 
    userExists, 
    writeln("Welcome back!"),
    notImplemented. % TODO: implement this

% Entry-point for Prolock.
% Either initiate newUserWorkflow or existingUserWorkflow
main :- newUserWorkflow ; existingUserWorkflow.