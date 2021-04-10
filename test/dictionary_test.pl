:- begin_tests(dictionary).
:- [src/dictionary].

% To run the tests:
% ?- run_tests.

test(hasKey) :- 
    not(hasKey("Bar", empty)),
    not(hasKey("Bar", dict("Foo", "Buz buzz", empty))),
    hasKey("Bar", dict("Bar", "Buz buzz", dict("Baz", "Boo", empty))),
    hasKey("Baz", dict("Bar", "Buz buzz", dict("Baz", "Boo", empty))),
    not(hasKey("Boo", dict("Bar", "Buz buzz", dict("Baz", "Boo", empty)))),
    not(hasKey("Boo", dict("Bar", "Buz buzz", dict("Baz", dict("Boo", "Fizz", empty), empty)))),
    hasKey("Boo", dict("Bar", "Buz buzz", dict("Boo", "Laa", dict("Baz", "Buzz", empty)))),
    not(hasKey("A", dict("Bar", dict("A", "B", empty), dict("Boo", "Laa", dict("Baz", "Buzz", empty))))),
    hasKey("Bar", dict("Bar", dict("A", "B", empty), dict("Boo", "Laa", dict("Baz", "Buzz", empty)))),
    hasKey("Boo", dict("Bar", dict("A", "B", empty), dict("Boo", dict("C", "D", empty), dict("Baz", "Buzz", empty)))).

test(value) :-
    not(value("Bar", "Buzz", empty)),
    not(value("Bar", "Buzz", dict("Foo", "Buz buzz", empty))),
    not(value("Bar", "Buzz", dict("Foo", "Buz buzz", dict("Bar", "Not Buzz", dict("Zoo", "Foo", empty))))),
    not(value("Boo", "Fizz", dict("Bar", "Buz buzz", dict("Baz", dict("Boo", "Fizz", empty), empty)))),
    value("Bar", "Buz buzz", dict("Bar", "Buz buzz", dict("Baz", "Boo", empty))),
    value("Baz", "Boo", dict("Bar", "Buz buzz", dict("Baz", "Boo", empty))),
    value("Bar", "Buzz", dict("Foo", "Buz buzz", dict("Bar", "Buzz", dict("Zoo", "Foo", empty)))),
    value("Bar", dict("A", "B", dict("C", "D", empty), empty), dict("Foo", "Buz buzz", dict("Bar", dict("A", "B", dict("C", "D", empty), empty), dict("Zoo", "Foo", empty)))).

test(insert) :-
    not(insert("Bar", "Foo", empty, empty)),
    insert("Bar", "Foo", empty, dict("Bar", "Foo", empty)),
    insert("Bar", "Foo", dict("Laa", "Baa", empty), dict("Laa", "Baa", dict("Bar", "Foo", empty))),
    insert("Bar", "Foo", dict("Bar", "Baa", dict("Laa", "Baa", empty)), dict("Bar", "Foo", dict("Laa", "Baa", empty))),
    insert("Bar", dict("A", "B", empty), dict("Bar", "Baa", dict("Laa", "Baa", empty)), dict("Bar", dict("A", "B", empty), dict("Laa", "Baa", empty))),
    insert("K", dict("A", "B", empty), dict("Bar", "Baa", dict("Laa", "Baa", empty)), dict("Bar", "Baa", dict("Laa", "Baa", dict("K", dict("A", "B", empty), empty)))).

test(remove) :-
    remove("Bar", empty, empty),
    remove("Bar", dict("Fizz", "Buzz", empty), dict("Fizz", "Buzz", empty)),
    remove("Bar", dict("Fizz", dict("Buzz", "Lightning", empty), empty), dict("Fizz", dict("Buzz", "Lightning", empty), empty)),
    remove("Bar", 
        dict("Baz", "Laa", dict("Bam", "Boo", dict("Bar", "Haha", dict("Laa", "Zaa", empty)))),
        dict("Baz", "Laa", dict("Bam", "Boo", dict("Laa", "Zaa", empty)))
    ),
    remove("Baz", 
        dict("Baz", "Laa", dict("Bam", "Boo", dict("Bar", "Haha", dict("Laa", "Zaa", empty)))),
        dict("Bam", "Boo", dict("Bar", "Haha", dict("Laa", "Zaa", empty)))
    ),
    remove("Baz", 
        dict("Baz", dict("A", "B", empty), dict("Bam", "Boo", dict("Bar", "Haha", dict("Laa", "Zaa", empty)))),
        dict("Bam", "Boo", dict("Bar", "Haha", dict("Laa", "Zaa", empty)))
    ).

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

:- initialization(run_tests, main).
