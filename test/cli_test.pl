:- begin_tests(cli).
:- [src/cli].

% To run the tests:
% ?- run_tests.

test(concatList) :-
    concatList(["hello"], "hello"),
    concatList(["foo!", "bar$", "bizz&"], "foo!bar$bizz&").

:- end_tests(cli).

:- initialization(run_tests, main).