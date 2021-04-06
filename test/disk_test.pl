:- begin_tests(disk).
:- [src/disk].

% To run the tests:
% ?- run_tests.

test(exists) :- 
    exists('test/resources/sample_file'), % returns true for an existing file
    not(exists('test/resources/does_not_exist')). % returns false for non-existing file

:- end_tests(disk).
