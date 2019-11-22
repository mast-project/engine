:- module(utils, [id//1]).

id(Id) --> idchars(L), { atom_codes(Id, L) }.

idchars([C|T]) -->
    [C],
    { code_type(C, csym) }, !,
    idchars(T).
idchars([]) --> [].
