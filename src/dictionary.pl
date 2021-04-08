:- module(prolock_dict).
:- [src/errors].
:- use_module(library(http/json)).

%% A key-value dictionary, where the value can be another dictionary %%

% A dictionary is:
% empty
% dict(Key, Value, Dict) where Key-Value is a pair in dict, Dict is the rest of the dictionary
% a dictionary does not have duplicate keys

% True if the dictionary has no values
dictIsEmpty(empty). % only true for empty


% True if the dictionary has the key `Key`
% TODO: implement this (1 hour) - Charles
hasKey(Key, dict(K,V,D)) :- hasKey(Key, D).
hasKey(Key, dict(Key,_,_)) :- true.


% True if `Value` is the value paired with `Key` in the dictionary
% TODO: implement this (1 hour) - Charles
value(Key, Value, dict(K,V,D)) :- value(Key, Value, D).
value(Key, Value, dict(Key,Value,D)) :- true.


% True if `NewDict` is the dictionary `Dict` with `Key` and `Value` added
% TODO: implement this (1 hour) - Charles
insert(Key, Value, empty,           dict(Key, Value, empty)) :- true.
insert(Key, Value, dict(Key, V, D), dict(Key, Value, D)) :- true.
insert(Key, Value, dict(K,   V, D), dict(K,   V,     D2)) :- insert(Key, Value, D, D2).


% True if `NewDict` is the dictionary `Dict` with `Key` removed
% If Dict does not have `Key`, then NewDict = Dict
% TODO: implement this (1 hour) - Charles
remove(Key, dict(Key, V, D), D) :- true.
remove(Key, dict(K, V, D), dict(K, V, D2)) :- remove(Key, D, D2).
remove(Key, Dict, Dict) :- not(hasKey(Key, Dict)).  


% True if the given variable is a dictionary
isDict(empty).
isDict(dict(Key,Value,Rest)) :- string(Key), (string(Value) ; isDict(Value)), isDict(Rest).


% True if String is the string representation of Dict
dictToString(Dict, String) :- nonvar(Dict), isDict(Dict), term_string(Dict, String), !.


% True if Dict is the dictionary parsed from String
stringToDict(Dict, String) :- nonvar(String), term_string(Dict, String), isDict(Dict), !.