:- module(prolock_dict).
:- [src/errors].
:- use_module(library(http/json)).

%%%%%%%%%%%%%%%%%%%%%%%%%%
% A key-value dictionary %
%%%%%%%%%%%%%%%%%%%%%%%%%%


% A dictionary is:
% empty
% dict(Key, Value, Dict) where Key-Value is a pair in dict, Dict is the rest of the dictionary
% Value can be string or another dictionary


% True if the dictionary has no values
dictIsEmpty(empty). % only true for empty


% True if the dictionary has the key `Key`
% TODO: implement this (1 hour) - Charles
hasKey(Key, dict(K,V,D)) :- notImplemented.


% True if `Value` is the value paired with `Key` in the dictionary
% TODO: implement this (1 hour) - Charles
value(Key, Value, dict(K,V,D)) :- notImplemented.


% True if `NewDict` is the dictionary `Dict` with `Key` and `Value` added
% TODO: implement this (1 hour) - Charles
insert(Key, Value, Dict, NewDict) :- notImplemented.


% True if `NewDict` is the dictionary `Dict` with `Key` removed
% If Dict does not have `Key`, then NewDict = Dict
% TODO: implement this (1 hour) - Charles
remove(Key, Dict, NewDict) :- notImplemented.


% True if the given variable is a dictionary
isDict(empty).
isDict(dict(Key,Value,Rest)) :- string(Key), (string(Value) ; isDict(Value)), isDict(Rest).


% True if String is the string representation of Dict
dictToString(Dict, String) :- nonvar(Dict), isDict(Dict), term_string(Dict, String), !.


% True if Dict is the dictionary parsed from String
stringToDict(Dict, String) :- nonvar(String), term_string(Dict, String), isDict(Dict), !.