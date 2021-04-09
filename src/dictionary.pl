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
<<<<<<< HEAD
% TODO: implement this (1 hour) - Charles
hasKey(Key, dict(K,V,D)) :- dif(Key,K), hasKey(Key, D).
=======
hasKey(Key, dict(K,V,D)) :- dif(Key, K), hasKey(Key, D).
>>>>>>> b09cdeeb98f87c9adac3b90d6e09844807a5273d
hasKey(Key, dict(Key,_,_)).


% True if `Value` is the value paired with `Key` in the dictionary
<<<<<<< HEAD
% TODO: implement this (1 hour) - Charles
value(Key, Value, dict(K,V,D)) :- value(Key, Value, D).
value(Key, Value, dict(Key,Value,D)).
=======
value(Key, Value, dict(K,V,D)) :- dif(Key, K), value(Key, Value, D).
value(Key, Value, dict(Key,Value,D)) :- true.
>>>>>>> b09cdeeb98f87c9adac3b90d6e09844807a5273d


% True if `NewDict` is the dictionary `Dict` with `Key` and `Value` added
% If key already exists, replaces the value
insert(Key, Value, empty, dict(Key, Value, empty)).
insert(Key, Value, dict(Key, V, D), dict(Key, Value, D)).
insert(Key, Value, dict(K, V, D), dict(K, V, D2)) :- dif(Key, K), insert(Key, Value, D, D2).


% True if `NewDict` is the dictionary `Dict` with `Key` removed
% If Dict does not have `Key`, then NewDict = Dict
remove(Key, dict(Key, V, D), D).
remove(Key, dict(K, V, D), dict(K, V, D2)) :- dif(Key, K), remove(Key, D, D2).
remove(Key, empty, empty).  


% True if the given variable is a dictionary
isDict(empty).
isDict(dict(Key,Value,Rest)) :- string(Key), (string(Value) ; isDict(Value)), isDict(Rest).


% True if String is the string representation of Dict
dictToString(Dict, String) :- nonvar(Dict), isDict(Dict), term_string(Dict, String), !.


% True if Dict is the dictionary parsed from String
stringToDict(Dict, String) :- nonvar(String), term_string(Dict, String), isDict(Dict), !.
