:- begin_tests(dictionary).
:- [src/dictionary].

% To run the tests:
% ?- run_tests.


test(dictToString) :-
    dictToString(empty, "empty"),
    dictToString(dict("foo", "bar", empty), "dict(\"foo\",\"bar\",empty)"),
    dictToString(
        dict("foo", dict("Bar", "Buz buzz", empty), dict("Fizz", "Bar foo", empty)), 
        "dict(\"foo\",dict(\"Bar\",\"Buz buzz\",empty),dict(\"Fizz\",\"Bar foo\",empty))"
    ).

test(stringToDict) :-
    stringToDict(empty, "empty"),
    stringToDict(dict("foo", "bar", empty), "dict(\"foo\",\"bar\",empty)"),
    stringToDict(
        dict("foo", dict("Bar", "Buz buzz", empty), dict("Fizz", "Bar foo", empty)), 
        "dict(\"foo\",dict(\"Bar\",\"Buz buzz\",empty),dict(\"Fizz\",\"Bar foo\",empty))"
    ).

test(dict_string_dict) :-
    Dict = dict("foo", dict("Bar", "Buz buzz", empty), dict("Fizz", "Bar foo", empty)),
    dictToString(Dict, String),
    stringToDict(Dict, String).


:- end_tests(dictionary).
