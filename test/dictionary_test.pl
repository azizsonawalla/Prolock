:- begin_tests(dictionary).
:- [src/dictionary].

% To run the tests:
% ?- run_tests.

test(asPrologDict) :- 
    asPrologDict(empty, prologDict{}),
    asPrologDict("a string", "a string"),
    asPrologDict(dict("Hello", "World", empty), prologDict{'Hello': "World"}),
    asPrologDict(
        dict("foo", dict("Bar", "Buz buzz", empty), dict("Fizz", "Bar foo", empty)), 
        prologDict{'Fizz':"Bar foo", foo:prologDict{'Bar':"Buz buzz"}}
    ).


:- end_tests(dictionary).
