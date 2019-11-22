:- module(alphabet, [unicode_cluster//1,
                     unicode_clusters/2,
                     unicode_clusters//1,
                     match_cluster/3,
                     grapheme//2, graphemes//2,
                     define_base_letter/2,
                     define_modifier/2,
                     define_text_token/5,
                     define_letter_class/3,
                     grapheme_pattern//2,
                     grapheme_patsplit/4,
                     parse_single_grapheme//2,
                     parse_grapheme_pattern//2]).
:- use_module(library(unicode)).
:- use_module(library(lists)).
:- use_module(utils).

:- dynamic base_letter/2, modifier/2, text_token/5,
           letter_class/3.

unicode_cluster(L) -->
    [C],
    unicode_cluster_extend(T),
    { unicode_nfkd([C | T], N),
      atom_codes(N, L) }.

unicode_cluster_extend([C | T]) -->
    [C],
    { unicode_property(C, extend(true)) }, !,
    unicode_cluster_extend(T).
unicode_cluster_extend([]) --> [].

unicode_clusters([C|T]) -->
    unicode_cluster(C), !,
    unicode_clusters(T).

unicode_clusters([]) --> [].

unicode_clusters(X, L) :-
    unicode_nfkd(X, N),
    atom_codes(N, NL),
    phrase(unicode_clusters(L), NL).

match_cluster([C1 | T1], [C2 | T2], Rem) :-
    unicode_property(C1, extend(false)), !,
    C1 == C2,
    match_cluster(T1, T2, Rem).

match_cluster([], Rem, Rem) :- !.
match_cluster([M1 | T], L2, Rem) :-
    selectchk(M1, L2, L2D), !,
    match_cluster(T, L2D, Rem).

grapheme(Alph, G) -->
    next_token(Alph, G), !.

grapheme(Alph, G) -->
    base_grapheme(Alph, B),
    modifiers(Alph, M),
    { append(B, M, G) }.

next_token(Alph, G) -->
    { text_token(Alph, _, Pfx, Classes, Sfx) },
    clusters(Pfx),
    classes(Classes, L), { L \== [] }, clusters(Sfx),
    { append(Pfx, L, G0), append(G0, Sfx, G) }.

clusters([C | T]) -->
    unicode_cluster(C), !,
    clusters(T).

clusters([]) --> [].

classes(Classes, [[C] | T]) -->
    [C],
    { member(Cls, Classes), unicode_property(C, category(Cls)) }, !,
    classes(Classes, T).

classes(_, []) --> [].

base_grapheme(Alph, B) -->
    { base_letter(Alph, B) },
    is_base_letter(B).

is_base_letter([G]), Rem --> !,
    unicode_cluster(G0),
    { match_cluster(G, G0, Rem) }.

is_base_letter([G | T]) -->
    unicode_cluster(G),
    is_base_letter(T).

modifiers(Alph, [M | T]) -->
    single_modifier(Alph, M),
    modifiers(Alph, T).

modifiers(_, []) --> [].

single_modifier(Alph, Mod), M0 -->
    { modifier(Alph, Mod) },
    unicode_cluster(M),
    { match_cluster(Mod, M, M0) }.

graphemes(Alph, [G | T]) -->
    grapheme(Alph, G),
    graphemes(Alph, T).

graphemes(_, []) --> [].

define_base_letter(Alph, A) :-
    unicode_clusters(A, NL),
    assertz(base_letter(Alph, NL)).

define_modifier(Alph, M) :-
    unicode_clusters(M, [NL]),
    assertz(modifier(Alph, NL)).

define_text_token(Alph, Id, Pfx, Classes, Sfx) :-
    unicode_clusters(Pfx, PfxL),
    unicode_clusters(Sfx, SfxL),
    assertz(text_token(Alph, Id, PfxL, Classes, SfxL)).

define_letter_class(Alph, Id, Alts) :-
    assertz(letter_class(Alph, Id, Alts)).

grapheme_pattern(exact(G), [G | T], T) --> [G].
grapheme_pattern(approx([GH | GT]), [[GH | GT0] | T], T) -->
    [[GH | GT0]],
    { subset(GT, GT0) }.
grapheme_pattern(contains(C), [G | T], T) -->
    [G],
    { memberchk(C, G) }.

grapheme_pattern(token(Pfx, Cls, Sfx), [G | T], T) -->
    [G],
    { phrase(check_token(Pfx, Cls, Sfx), G, []) }.

grapheme_pattern(oneof(L), [G | T], T) -->
    [G],
    { memberchk(G, L) }.

grapheme_pattern((P1, P2), H, T0) -->
    grapheme_pattern(P1, H, T),
    grapheme_pattern(P2, T, T0).

grapheme_pattern((P1; P2), H, T) -->
    grapheme_pattern(P1, H, T),
    grapheme_pattern(P2, H, T).

grapheme_pattern(P1 / P2, H, T), TC -->
    grapheme_pattern(P1, H, T),
    grapheme_pattern(P2, TC, []).

grapheme_pattern(?(P), H, T) -->
    grapheme_pattern(P, H, T).

grapheme_pattern(?(_), H, H) --> [].

grapheme_pattern(*(P), H, T0) -->
    grapheme_pattern(P, H, T), !,
    grapheme_pattern(*(P), T, T0).

grapheme_pattern(*(_), H, H) --> [].

grapheme_pattern(same(N, P), H, T) -->
    grapheme_pattern(P, H, T0),
    { N1 is N - 1, copy_term(H:T0, H0:[]) },
    repeatn(N1, H0, T0, T).

grapheme_pattern(P, H) --> grapheme_pattern(P, H, []).

repeatn(N, _, H, H) --> { N =< 0 }, !.
repeatn(N, L, H, T) -->
    exact(L, H, T0),
    { N1 is N - 1 },
    repeatn(N1, L, T0, T).

exact([], H, H) --> [].
exact([C | L], [C | T], T0) -->
    [C],
    exact(L, T, T0).

grapheme_patsplit(P, X, H, T) :-
    phrase(grapheme_pattern(P, H), X, T).

check_token(Pfx, Cls, Sfx) -->
    Pfx,
    check_token_middle(Cls),
    Sfx.

check_token_middle(Cls) -->
    [[C]],
    { member(Cat, Cls),
      unicode_property(C, category(Cat)) }, !,
    check_token_middle(Cls).

check_token_middle(_) --> [].

parse_single_grapheme(Alph, G) -->
    `\\`, !, grapheme(Alph, G).

parse_single_grapheme(Alph, G) -->
    `'`, grapheme(Alph, G), `'`, !.

parse_single_grapheme(Alph, G) -->
    \+ separator,
    grapheme(Alph, G).

separator --> [C], { code_type(C, space) }.
separator --> `,`.
separator --> `;`.

pat_atomic_grapheme(Alph, approx(G)) -->
    `~`, !, parse_single_grapheme(Alph, G).
pat_atomic_grapheme(_, contains(C)) -->
    `_`, !, unicode_cluster(C).
pat_atomic_grapheme(Alph, P) -->
    `(`, parse_grapheme_pattern(Alph, P), `)`, !.
pat_atomic_grapheme(Alph, ?(P)) -->
    `[`, parse_grapheme_pattern(Alph, P), `]`, !.
pat_atomic_grapheme(Alph, X) -->
    `{`, id(Id), `}`, { class_or_token(Alph, Id, X) }, !.
pat_atomic_grapheme(Alph, exact(G)) -->
    parse_single_grapheme(Alph, G).

class_or_token(Alph, Id, token(Pfx, Cls, Sfx)) :-
    text_token(Alph, Id, Pfx, Cls, Sfx), !.

class_or_token(Alph, Id, oneof(L)) :-
    letter_class(Alph, Id, L).

pat_repeat(Alph, *(P)) -->
    pat_atomic_grapheme(Alph, P), `*`, !.
pat_repeat(Alph, same(N, P)) -->
    pat_atomic_grapheme(Alph, P),
    [C], { code_type(C, digit(N)) }, !.
pat_repeat(Alph, P) -->
    pat_atomic_grapheme(Alph, P).

pat_sequence(Alph, (P1, P2)) -->
    pat_repeat(Alph, P1),
    pat_sequence(Alph, P2), !.
pat_sequence(Alph, P) --> pat_repeat(Alph, P).

pat_lookahead(Alph, P1 / P2) -->
    pat_sequence(Alph, P1), `/`, !,
    pat_sequence(Alph, P2).

pat_lookahead(Alph, P) -->
    pat_sequence(Alph, P).

parse_grapheme_pattern(Alph, (P1; P2)) -->
    pat_lookahead(Alph, P1), `|`, !,
    parse_grapheme_pattern(Alph, P2).

parse_grapheme_pattern(Alph, P) -->
    pat_lookahead(Alph, P).
