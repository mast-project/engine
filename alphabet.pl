:- module(alphabet, [unicode_cluster//1, match_clusters/3,
                     grapheme//2, define_base_letter/2,
                     define_modifier/2]).
:- use_module(library(unicode)).
:- use_module(library(lists)).
:- use_module(library(apply)).

:- dynamic base_letter/2, modifier/2.

unicode_cluster(N) -->
    [C],
    unicode_cluster_extend(T),
    { unicode_nfkd([C | T], N) }.

unicode_cluster_extend([C | T]) -->
    [C],
    { unicode_property(C, extend(true)) }, !,
    unicode_cluster_extend(T).
unicode_cluster_extend([]) --> [].

match_clusters(A1, A2, Rem) :-
    atom_codes(A1, L1),
    atom_codes(A2, L2),
    match_clusters0(L1, L2, Rem).

is_extend(C) :- unicode_property(C, extend(true)).

match_clusters0([C1 | T1], [C1 | T2], Rem) :-
    unicode_property(C1, extend(false)), !,
    match_clusters0(T1, T2, Rem).

match_clusters0([], Rem, Rem) :-
    maplist(is_extend, Rem), !.
match_clusters0([M1 | T], L2, Rem) :-
    selectchk(M1, L2, L2D), !,
    match_clusters0(T, L2D, Rem).

unicode_clusters(N) --> unicode_cluster(N).
unicode_clusters(N1) -->
    unicode_cluster(N),
    unicode_clusters(T),
    { atom_concat(N, T, N1) }.

grapheme(Alph, X) -->
    base_grapheme(Alph, B),
    modifiers(Alph, M),
    { atom_concat(B, M, X) }.

base_grapheme(Alph, Base), M0 -->
    unicode_clusters(N),
    { base_letter(Alph, Base), match_clusters(Base, N, M0) }.

modifiers(Alph, M) -->
    single_modifier(Alph, M1),
    modifiers(Alph, M2),
    { atom_concat(M1, M2, M) }.

modifiers(_, '') --> [].

single_modifier(Alph, Mod), M0 -->
    unicode_cluster(N),
    { modifier(Alph, Mod), match_clusters(Mod, N, M0) }.

define_base_letter(Alph, A) :-
    unicode_nfkd(A, N),
    assertz(base_letter(Alph, N)).

define_modifier(Alph, M) :-
    unicode_nfkd(M, N),
    assertz(modifier(Alph, N)).
