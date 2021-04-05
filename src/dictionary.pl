:- [errors].
:- dynamic notImplemented/0.

%% A key-value dictionary, where the value can be another dictionary %%

% A dictionary is:
% empty
% dict(Key, Value, Dict) where Key-Value is a pair in dict, Dict is the rest of the dictionary


% True if the dictionary has no values
dictIsEmpty(empty). % only true for empty


% True if the dictionary has the key `Key`
hasKey(Key, dict(K,V,D)) :- notImplemented. % TODO: implement


% True if `Value` is the value paired with `Key` in the dictionary
value(Key, Value, dict(K,V,D)) :- notImplemented. % TODO: implement


% True if `NewDict` is the dictionary `Dict` with `Key` and `Value` added
insert(Key, Value, Dict, NewDict) :- notImplemented. % TODO: implement


% True if `NewDict` is the dictionary `Dict` with `Key` removed
% If Dict does not have `Key`, then NewDict = Dict
remove(Key, Dict, NewDict) :- notImplemented. % TODO: implement