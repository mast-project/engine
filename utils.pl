:- module(utils, [id//1, range/4]).

id(Id) --> idchars(L), { atom_codes(Id, L) }.

idchars([C|T]) -->
    [C],
    { code_type(C, csym) }, !,
    idchars(T).
idchars([]) --> [].

range(N, inf, L, L0) :- !,
    N1 is N - 1,
    length(Pref, N1),
    append(Pref, L0, L).
range(1, N, L, L0) :-
    length(L0, N),
    append(L0, _, L), !.
range(1, _, L, L) :- !.

range(N, M, [_ | L], L0) :-
    N1 is N - 1,
    M1 is M - 1,
    range(N1, M1, L, L0), !.

range(_, _, _, []).
