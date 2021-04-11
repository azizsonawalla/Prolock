% True if String is the concatenation of all strings in List
concatList(List,String) :-
    reverse(List, Reverse),
    foldl(concat, Reverse, "", StringOrAtom),
    atom_string(StringOrAtom, String).   