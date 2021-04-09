:- begin_tests(disk).
:- [src/disk].
:- [src/crypto].

% To run the tests:
% ?- run_tests.


test(write_read) :-
    hash("this is some message", Hash),
    Filename = "test/resources/test.txt",
    writeData(Hash, Filename),
    readData(HashFromDisk, Filename),
    HashFromDisk = Hash,
    delete(Filename).


test(exists) :- 
    exists('test/resources/sample_file'), % returns true for an existing file
    not(exists('test/resources/does_not_exist')). % returns false for non-existing file

:- end_tests(disk).

:- initialization(run_tests, main).