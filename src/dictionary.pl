:- module(prolock_dict).
:- [src/errors].
:- use_module(library(http/json)).

%% A key-value dictionary, where the value can be another dictionary %%

% A dictionary is:
% empty
% dict(Key, Value, Dict) where Key-Value is a pair in dict, Dict is the rest of the dictionary


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


% True if String is the string representation of Dict
dictToString(Dict, String) :- 
    asPrologDict(Dict, PrologDict),
    term_string(PrologDict, String).


% True if Dict is the dictionary parsed from String
% TODO: implement this (2 hour) - Aziz
stringToDict(Dict, String) :- notImplemented.


% True if PrologDict is the Prolog Dictionary version of Dict
% TODO: Aziz
asPrologDict(empty, pdict{}) :- !.
asPrologDict(String, String) :- string(String), !.
asPrologDict(dict(Key, Value, RestDict), PrologDict) :- 
    convert_to_atom(Key,KeyAtom),
    asPrologDict(RestDict, RestPrologDict),
    asPrologDict(Value, ValuePrologDict),
    PrologDict = RestPrologDict.put(KeyAtom, ValuePrologDict).

convert_to_atom(X,Atom)  :- nonvar(X),!,atom_string(Atom,X).
convert_to_atom(X,_ )    :- var(X),!,instantiation_error(X).