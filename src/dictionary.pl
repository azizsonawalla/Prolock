:- module(prolock_dict).
:- [src/errors].

%%%%%%%%%%%%%%%%%%%%%%%%%%
% A key-value dictionary %
%%%%%%%%%%%%%%%%%%%%%%%%%%


% A dictionary is:
% empty
% dict(Key, Value, Dict) where Key-Value is a pair in dict, Dict is the rest of the dictionary
% a dictionary does not have duplicate keys
% Value can be string or another dictionary


% True if the dictionary has no values
dictIsEmpty(empty). % only true for empty


% True if the dictionary has the key `Key`
hasKey(Key, dict(K,_,D)) :- 
    (Key == K -> true
    ; hasKey(Key, D)
    ).


% True if `Value` is the value paired with `Key` in the dictionary
% value(Key, Value, dict(K,V,D)) :- dif(Key, K), value(Key, Value, D).
% value(Key, Value, dict(Key,Value,D)) :- true.
value(Key, Value, dict(K,V,D)) :- 
    (dif(Key, K) -> value(Key, Value, D)
    ; Value = V
    ).


% True if `NewDict` is the dictionary `Dict` with `Key` and `Value` added
% If key already exists, replaces the value
insert(Key, Value, D, Result) :-
    (D == empty -> Result = dict(Key, Value, empty)  % insert into empty dictionary
    ; (D = dict(K1, _, D1), K1 == Key -> Result = dict(Key, Value, D1) % if keys are same, replace value
      ; D = dict(K2, V2, D2), insert(Key, Value, D2, InsertedD), Result = dict(K2, V2, InsertedD) % keys are different
      ) 
    ).



% True if `NewDict` is the dictionary `Dict` with `Key` removed
% If Dict does not have `Key`, then NewDict = Dict
remove(Key, D, Result) :- 
    (D == empty -> Result = empty
    ; (D = dict(K1, _, D1), K1 == Key -> Result = D1 % if keys are same, remove the pair
      ; D = dict(K2, V2, D2), remove(Key, D2, RemovedD) -> Result = dict(K2, V2, RemovedD) % keys are different
      )
    ).


% True if the given variable is a dictionary
isDict(empty).
isDict(dict(Key,Value,Rest)) :- string(Key), (string(Value) ; isDict(Value)), isDict(Rest).


% True if String is the string representation of Dict
dictToString(Dict, String) :- nonvar(Dict), isDict(Dict), term_string(Dict, String), !.


% True if Dict is the dictionary parsed from String
stringToDict(Dict, String) :- nonvar(String), term_string(Dict, String), isDict(Dict), !.
