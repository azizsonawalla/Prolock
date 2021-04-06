:- begin_tests(dictionary).
:- [src/dictionary].

% To run the tests:
% ?- run_tests.

test(asPrologDict) :- 
    asPrologDict(empty, pdict{}),
    asPrologDict("a string", "a string"),
    asPrologDict(dict("Hello", "World", empty), pdict{'Hello': "World"}),
    asPrologDict(
        dict("foo", dict("Bar", "Buz buzz", empty), dict("Fizz", "Bar foo", empty)), 
        pdict{'Fizz':"Bar foo", foo:pdict{'Bar':"Buz buzz"}}
    ).


test(dictToString) :-
    dictToString(empty, "pdict{}"),
    dictToString(dict("foo", "bar", empty), "pdict{ foo: \"bar\"}"),
    dictToString(
        dict("foo", dict("Bar", "Buz buzz", empty), dict("Fizz", "Bar foo", empty)), 
        "pdict{'Fizz':\"Bar foo\",foo:pdict{'Bar':\"Buz buzz\"}}"
    ).


:- end_tests(dictionary).
