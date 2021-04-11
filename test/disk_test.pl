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


test(write_read_hex_bytes) :-
    Bytes = [46, 227, 167, 94, 25, 11, 85, 47, 90, 10, 189, 155, 115, 125, 87, 161],
    Filename = "test.txt",
    writeHexBytes(Bytes, Filename),
    readHexBytes(BytesFromDisk, Filename),
    BytesFromDisk = Bytes,
    delete(Filename).


test(exists) :- 
    exists('test/resources/sample_file'), % returns true for an existing file
    not(exists('test/resources/does_not_exist')). % returns false for non-existing file

:- end_tests(disk).

:- initialization(run_tests, main).