:- module(alphabet, [unicode_cluster//1,
                     unicode_cluster_nfd//1,
                     grapheme//2, graphemes//2,
                     grapheme_word//2,
                     define_base_letter/2,
                     define_modifier/2,
                     define_text_token/5,
                     define_letter_class/3,
                     define_transform_rule/5,
                     define_rec_transform_rule/4,
                     grapheme_pattern//3,
                     grapheme_match/2,
                     grapheme_patsplit/4,
                     grapheme_patsuffix/4,
                     grapheme_patsubstr/5,
                     parse_single_grapheme//2,
                     parse_grapheme_pattern//2,
                     parse_grapheme_pattern/3,
                     parse_grapheme_simple_pattern//2,
                     parse_grapheme_simple_pattern/3,
                     graphemes_string/3,
                     valid_grapheme/2,
                     enum_graphemes/3,
                     grapheme_replace/3,
                     parse_grapheme_subst//2,
                     parse_grapheme_subst/3,
                     transform_graphemes/4,
                     transform_target/3,
                     enum_transforms/3]).
:- use_module(library(dcg/basics)).
:- use_module(library(unicode)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(utils).

:- dynamic base_letter/2, modifier/2, text_token/5,
           letter_class/3, transform_rule/6, nfd_cache/2.

nfd_codes(X, L) :-
    nfd_cache(X, L), !.

nfd_codes(X, L) :-
    unicode_nfd(X, N),
    atom_codes(N, L),
    assertz(nfd_cache(X, L)).

unicode_cluster([C | T]) -->
    [C],
    unicode_cluster_extend(T).

unicode_cluster_extend([C | T]) -->
    unicode_extend_char(C), !,
    unicode_cluster_extend(T).
unicode_cluster_extend([]) --> [].

unicode_extend_char(C) -->
    [C],
    { unicode_property(C, extend(true)) }.

unicode_cluster_nfd(NL) -->
    unicode_cluster(L),
    { nfd_codes(L, NL) }.

sequence([]) --> [], !.
sequence(B) -->
    unicode_cluster_nfd(C),
    { append(C, B0, B) }, !,
    sequence(B0).
sequence([BH|BT]), Rem -->
    unicode_cluster_nfd([BH|T]),
    { subset_diff(BT, T, Rem) }.

subset_diff([], X, X).
subset_diff([H | T], L, R) :-
    selectchk(H, L, L0),
    subset_diff(T, L0, R).

grapheme(Alph, token(Name, Content)) -->
    next_token(Alph, Name, Content), !.

grapheme(Alph, grapheme(B, M)) -->
    base_grapheme(Alph, B),
    modifiers(Alph, M),
    \+ unicode_extend_char(_).

next_token(Alph, Name, Contents) -->
    { text_token(Alph, Name, Pfx, SubAlph, Sfx) },
    sequence(Pfx),
    graphemes(SubAlph, Contents),
    sequence(Sfx).

base_grapheme(Alph, B) -->
    { base_letter(Alph, B) },
    sequence(B).

modifiers(Alph, [M | T]) -->
    modifier(Alph, M),
    modifiers(Alph, T).

modifiers(_, []) --> [].

modifier(Alph, Mod) -->
    { modifier(Alph, Mod) },
    sequence(Mod).

graphemes(Alph, [G | T]) -->
    grapheme(Alph, G),
    graphemes(Alph, T).

graphemes(_, []) --> [].

grapheme_word(Alph, [G | T]) -->
    \+ separator, !,
    grapheme(Alph, G),
    grapheme_word(Alph, T).

grapheme_word(_, []) --> [].

define_base_letter(Alph, A) :-
    nfd_codes(A, NL),
    assertz(base_letter(Alph, NL)).

define_modifier(Alph, M) :-
    nfd_codes(M, NL),
    assertz(modifier(Alph, NL)).

define_text_token(Alph, Id, Pfx, SubAlph, Sfx) :-
    nfd_codes(Pfx, PfxL),
    nfd_codes(Sfx, SfxL),
    assertz(text_token(Alph, Id, PfxL, SubAlph, SfxL)).

define_letter_class(Alph, Id, Alts) :-
    assertz(letter_class(Alph, Id, Alts)).

define_transform_rule(Id, SAlph, P, DAlph, R) :-
    assertz(transform_rule(Id, SAlph, P, DAlph, R, false)).

define_rec_transform_rule(Id, SAlph, P, R) :-
    assertz(transform_rule(Id, SAlph, P, SAlph, R, true)).

grapheme_pattern(_, fence, X, X) --> eos, !.
grapheme_pattern(true, fence, X, X) --> [].

grapheme_pattern(_, null, X, X) --> [].

grapheme_pattern(_, any, [G | T], T) -->
    [G].

grapheme_pattern(_, exact(G), [G | T], T) -->
    [G].

grapheme_pattern(_, approx(grapheme(B, M)), [grapheme(B, M0) | T], T) -->
    [grapheme(B, M0)],
    { subset(M, M0) }.

grapheme_pattern(_, approx(token(N, L)), [token(N, L0) | T], T) -->
    [token(N, L0)],
    { append(L, _, L0) }.

grapheme_pattern(_, token(Name), [token(Name, Contents) | T], T) -->
    [token(Name, Contents)].

grapheme_pattern(Fence, oneof(Pats), H, T) -->
    { member(Pat, Pats) },
    grapheme_pattern(Fence, Pat, H, T).

grapheme_pattern(Fence, contains(M, SubP), [grapheme(B, ML) | T], T) -->
    [grapheme(B, ML)],
    { selectchk(M, ML, ML0),
      phrase(grapheme_pattern(Fence, SubP, _, _),
             [grapheme(B, ML0)]) }.

grapheme_pattern(Fence, (P1, P2), H, T0) -->
    grapheme_pattern(Fence, P1, H, T),
    {advance(Fence, H, T, Fence0)},
    grapheme_pattern(Fence0, P2, T, T0).

grapheme_pattern(Fence, (P1; P2), H, T) -->
    grapheme_pattern(Fence, P1, H, T),
    grapheme_pattern(Fence, P2, H, T).

grapheme_pattern(Fence, P1 / P2, H, T), TC -->
    grapheme_pattern(Fence, P1, H, T),
    {advance(Fence, H, T, Fence0)},
    grapheme_pattern(Fence0, P2, TC, []).

grapheme_pattern(Fence, P1 - P2, H, T) -->
    grapheme_pattern(Fence, P1, H, T),
    {advance(Fence, H, T, Fence0)},
    { copy_term(H:T, H0:[]),
      \+ phrase(grapheme_pattern(Fence0, P2, _), H0) }.
grapheme_pattern(Fence, ?(P), H, T) -->
    grapheme_pattern(Fence, P, H, T).

grapheme_pattern(_, ?(_), H, H) --> [].

grapheme_pattern(Fence, *(P), H, T0) -->
    grapheme_pattern(Fence, P, H, T),
    {advance(Fence, H, T, Fence0)},
    grapheme_pattern(Fence0, *(P), T, T0).

grapheme_pattern(_, *(_), H, H) --> [].

grapheme_pattern(Fence, same(N, P), H, T) -->
    grapheme_pattern(Fence, P, H, T0),
    { N1 is N - 1, copy_term(H:T0, H0:[]) },
    repeatn(N1, H0, T0, T).

grapheme_pattern(Fence, P, H) --> grapheme_pattern(Fence, P, H, []).

grapheme_match(P, X) :-
    phrase(grapheme_pattern(true, P, _), X).

advance(false, _, _, false) :- !.
advance(true, H, T, false) :- H \= T, !.
advance(true, _, _, true).

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
    phrase(grapheme_pattern(true, P, H), X, T).

grapheme_patsuffix(Fence, P, [], T) -->
    grapheme_pattern(Fence, P, T).

grapheme_patsuffix(_, P, [C | H], T) -->
    [C],
    grapheme_patsuffix(false, P, H, T).

grapheme_patsuffix(P, X, H, T) :-
    phrase(grapheme_patsuffix(true, P, H, T), X).

grapheme_patsubstr(P, X, H, T, A) :-
    phrase(grapheme_patsuffix(true, P, H, T), X, A).


parse_single_grapheme(Alph, G) -->
    `\\`, !, grapheme(Alph, G).

parse_single_grapheme(Alph, grapheme([C0], [])) -->
    `<C-`, [C],
    { code_type(C, lower),
      C0 is C - 96,
      base_letter(Alph, [C0])},
    `>`, !.

parse_single_grapheme(Alph, grapheme([C], [])) -->
    `<U+`, xinteger(C), `>`,
    { base_letter(Alph, [C])}, !.

parse_single_grapheme(Alph, G) -->
    `'`, grapheme(Alph, G), `'`, !.

parse_single_grapheme(Alph, G) -->
    \+ separator,
    grapheme(Alph, G).

pat_atomic_grapheme(_, fence) --> `#`.
pat_atomic_grapheme(Alph, approx(B, M)) -->
    `~`, !, parse_single_grapheme(Alph, grapheme(B, M)).
pat_atomic_grapheme(Alph, contains(C, any)) -->
    `_`, !, modifier(Alph, C).
pat_atomic_grapheme(Alph, P) -->
    `(`, parse_grapheme_pattern(Alph, P), `)`, !.
pat_atomic_grapheme(Alph, ?(P)) -->
    `[`, parse_grapheme_pattern(Alph, P), `]`, !.
pat_atomic_grapheme(Alph, X) -->
    `{`, id(Id), `}`, { class_or_token(Alph, Id, X) }, !.
pat_atomic_grapheme(Alph, contains(M, oneof(Pats))) -->
    `{`, id(Id), modifier(Alph, M), `}`,
    { letter_class(Alph, Id, Pats) }, !.
pat_atomic_grapheme(Alph, exact(G)) -->
    parse_single_grapheme(Alph, G).

class_or_token(Alph, Id, token(Id)) :-
    text_token(Alph, Id, _, _, _), !.

class_or_token(Alph, Id, oneof(Pats)) :-
    letter_class(Alph, Id, Pats).

pat_repeat(Alph, *(P)) -->
    pat_atomic_grapheme(Alph, P), `*`, !.
pat_repeat(Alph, same(N, P)) -->
    pat_atomic_grapheme(Alph, P),
    [C], { code_type(C, digit(N)), N > 1 }, !.
pat_repeat(Alph, P) -->
    pat_atomic_grapheme(Alph, P).

pat_sequence(_, null) --> `0`, !.
pat_sequence(Alph, (P1, P2)) -->
    pat_repeat(Alph, P1),
    pat_sequence(Alph, P2), !.
pat_sequence(Alph, P) --> pat_repeat(Alph, P).

pat_lookahead(Alph, P1 / P2) -->
    pat_sequence(Alph, P1), `/`, !,
    pat_sequence(Alph, P2).

pat_lookahead(Alph, P1 - P2) -->
    pat_sequence(Alph, P1), `\\`, !,
    pat_sequence(Alph, P2).

pat_lookahead(Alph, P) -->
    pat_sequence(Alph, P).

parse_grapheme_pattern(Alph, (P1; P2)) -->
    pat_lookahead(Alph, P1), `|`, !,
    parse_grapheme_pattern(Alph, P2).

parse_grapheme_pattern(Alph, P) -->
    pat_lookahead(Alph, P).

parse_grapheme_pattern(Alph, P, X) :-
    phrase(parse_grapheme_pattern(Alph, P), X).

pat_simple_grapheme(Alph, contains(C, any)) -->
    `_`, !, modifier(Alph, C).
pat_simple_grapheme(Alph, ?(P)) -->
    `[`, parse_grapheme_simple_pattern(Alph, P), `]`, !.
pat_simple_grapheme(Alph, X) -->
    `{`, id(Id), `}`, { class_or_token(Alph, Id, X) }, !.
pat_simple_grapheme(Alph, contains(M, oneof(Pats))) -->
    `{`, id(Id), modifier(Alph, M), `}`,
    { letter_class(Alph, Id, Pats) }, !.
pat_simple_grapheme(Alph, exact(G)) -->
    parse_single_grapheme(Alph, G), !.

pat_simple_repeat(Alph, *(P)) -->
    pat_simple_grapheme(Alph, P), `*`, !.
pat_simple_repeat(Alph, same(N, P)) -->
    pat_simple_grapheme(Alph, P),
    [C], { code_type(C, digit(N)), N > 1 }, !.
pat_simple_repeat(Alph, P) -->
    pat_simple_grapheme(Alph, P).

parse_grapheme_simple_pattern(_, null) --> `0`, !.
parse_grapheme_simple_pattern(Alph, (P1, P2)) -->
    pat_simple_repeat(Alph, P1),
    parse_grapheme_simple_pattern(Alph, P2), !.
parse_grapheme_simple_pattern(Alph, P) -->
    pat_simple_repeat(Alph, P).

parse_grapheme_simple_pattern(Alph, P, X) :-
    phrase(parse_grapheme_simple_pattern(Alph, P), X).

graphemes_string(Alph, G, N) :-
    phrase(graphemes_string(Alph, G), L),
    unicode_nfc(L, N).

graphemes_string(Alph, [H | T]) -->
    grapheme_string(Alph, H),
    graphemes_string(Alph, T).

graphemes_string(_, []) --> [].

grapheme_string(_, grapheme(B, M)) -->
    B,
    modifiers_string(M).

grapheme_string(Alph, token(Name, Content)) -->
    { text_token(Alph, Name, Pfx, SubAlph, Sfx) },
    Pfx, graphemes_string(SubAlph, Content), Sfx.

modifiers_string([M | T]) -->
    M,
    modifiers_string(T).

modifiers_string([]) --> [].

valid_grapheme(Alph, grapheme(B, M)) :-
    base_letter(Alph, B),
    maplist(modifier(Alph), M).
valid_grapheme(Alph, token(Name, Contents)) :-
    text_token(Alph, Name, _, SubAlph, _),
    maplist(valid_grapheme(SubAlph), Contents).

enum_graphemes(_, border) -->
    { domain_error(lookahead, border) }.
enum_graphemes(_, null) --> [].
enum_graphemes(_, exact(X)) --> [X].
enum_graphemes(Alph, any) -->
    { base_letter(Alph, B),
      gen_modifiers(Alph, [], ML) },
    [grapheme(B, ML)].
enum_graphemes(Alph, approx(grapheme(B, ML))) -->
    { gen_modifiers(Alph, ML, ML0) },
    [grapheme(B, ML0)].
enum_graphemes(_, approx(token(N, C))) -->
    { domain_error(infinite_pattern, approx(token(N, C))) }.
enum_graphemes(Alph, contains(M, P)) -->
    { phrase(enum_graphemes(Alph, P), [grapheme(B, ML)]),
      memberchk(M, ML) },
    [grapheme(B, ML)].
enum_graphemes(_, token(N)) -->
    { domain_error(infinite_pattern, token(N)) }.
enum_graphemes(Alph, oneof(Pats)) -->
    { member(P, Pats) },
    enum_graphemes(Alph, P).

enum_graphemes(Alph, (X, Y)) -->
    enum_graphemes(Alph, X),
    enum_graphemes(Alph, Y).
enum_graphemes(Alph, (X; Y)) -->
    enum_graphemes(Alph, X);
    enum_graphemes(Alph, Y).
enum_graphemes(_, X / Y) -->
    { domain_error(lookahead, X / Y) }.

enum_graphemes(Alph, X - Y) -->
    { enum_graphemes(Alph, X, L),
      \+ phrase(grapheme_pattern(_, Y, _), L) },
    L.

enum_graphemes(Alph, ?(X)) -->
    []; enum_graphemes(Alph, X).

enum_graphemes(_, *(X)) -->
    { domain_error(infinite_pattern, *(X)) }.

enum_graphemes(Alph, same(N, X)) -->
    { enum_graphemes(Alph, X, L) },
    repeatn(N, L, _, _).

enum_graphemes(Alph, P, X) :-
    phrase(enum_graphemes(Alph, P), X).

gen_modifiers(Alph, In, Out) :-
    findall(M, modifier(Alph, M), ML),
    subsets(In, ML, Out).
gen_modifiers(_, [], []).

subsets(In, S, Out) :-
    S \= [],
    subset(In, S),
    (Out = S;
     select(_, S, S0),
     subsets(In, S0, Out)).

grapheme_replace(null, _, _) --> [].

grapheme_replace((R1, R2), S, T) -->
    grapheme_replace(R1, S, T),
    grapheme_replace(R2, S, T).

grapheme_replace(exact(G), _, T) -->
    { apply_transform(T, G, G0) },
    [G0].

grapheme_replace(N-M, S, T) -->
    { range(N, M, S, L),
      maplist(apply_transform(T), L, L0) },
    L0.

grapheme_replace(-N, S, T) -->
    { length(Suff, N),
      append(Pref, Suff, S), !,
      maplist(apply_transform(T), Pref, Pref0) },
    Pref0.
grapheme_replace(-_, _, _) --> [].

grapheme_replace(upper(R), S, T) -->
    grapheme_replace(R, S, [upper | T]).

grapheme_replace(lower(R), S, T) -->
    grapheme_replace(R, S, [lower | T]).

grapheme_replace(add(R, M), S, T) -->
    grapheme_replace(R, S, [add(M) | T]).

grapheme_replace(remove(R, M), S, T) -->
    grapheme_replace(R, S, [remove(M) | T]).

grapheme_replace(chop(R), S, T) -->
    grapheme_replace(R, S, [chop | T]).

grapheme_replace(base(R), S, T) -->
    grapheme_replace(R, S, [base | T]).

grapheme_replace(apply(Id, Alphs, R), S, T) -->
    grapheme_replace(R, S, [apply(Id, Alphs) | T]).


grapheme_replace(repeat(0, _), _, _) --> !, [].
grapheme_replace(repeat(1, R), S, T) --> !,
    grapheme_replace(R, S, T).
grapheme_replace(repeat(N, R), S, T) -->
    grapheme_replace(R, S, T),
    { N1 is N - 1 },
    grapheme_replace(repeat(N1, R), S, T).

apply_transform([], X, X).
apply_transform([H | T], X, Y) :-
    apply_transform1(H, X, X0),
    apply_transform(T, X0, Y).

apply_transform1(upper, X, Y) :-
    toupper_grapheme(X, Y).

apply_transform1(lower, X, Y) :-
    tolower_grapheme(X, Y).

apply_transform1(chop, grapheme([_ | B], ML), grapheme(B, ML)) :- !.
apply_transform1(chop, X, X).

apply_transform1(base, grapheme(B, _), grapheme(B, [])) :- !.
apply_transform1(base, X, X).

apply_transform1(add(M), grapheme(B, ML), grapheme(B, [M | ML])) :-
    \+ memberchk(M, ML), !.
apply_transform1(add(_), X, X).

apply_transform1(remove(M), grapheme(B, ML), grapheme(B, ML0)) :-
    selectchk(M, ML, ML0), !.
apply_transform1(remove(_), X, X).

apply_transform1(apply(Id, Alphs), X, Y) :-
    transform_graphemes(Id, Alphs, [X], [Y]).

toupper_grapheme(grapheme(B, M), grapheme(BU, MU)) :-
    toupperl(B, BU),
    maplist(toupperl, M, MU).
toupper_grapheme(token(Name, C), token(Name, CU)) :-
    maplist(toupper_grapheme, C, CU).

tolower_grapheme(grapheme(B, M), grapheme(BL, ML)) :-
    tolowerl(B, BL),
    maplist(tolowerl, M, ML).
tolower_grapheme(token(Name, C), token(Name, CL)) :-
    maplist(tolower_grapheme, C, CL). 

toupperl(L, LU) :- maplist(toupperc, L, LU).

tolowerl(L, LU) :- maplist(tolowerc, L, LU).

toupperc(X, U) :- unicode_property(X, uppercase_mapping(U)), !.
toupperc(X, X).

tolowerc(X, U) :- unicode_property(X, lowercase_mapping(U)), !.
tolowerc(X, X).

grapheme_replace(R, S, S1) :-
    phrase(grapheme_replace(R, S, []), S1).

parse_grapheme_subst(_, null) --> `0`, !.
parse_grapheme_subst(Alphs, (P1, P2)) -->
    parse_subst_repeat(Alphs, P1),
    parse_grapheme_subst(Alphs, P2), !.
parse_grapheme_subst(Alphs, P) -->
    parse_subst_repeat(Alphs, P), !.

parse_subst_repeat(Alphs, repeat(N, P)) -->
    parse_subst_elem(Alphs, P),
    [C], { code_type(C, digit(N)), N > 1 }, !.

parse_subst_repeat(Alphs, P) -->
    parse_subst_elem(Alphs, P).

parse_subst_elem(Alphs, P) -->
    parse_subst_var(Alphs, P), !.
parse_subst_elem(_/Alph, exact(G)) -->
    parse_single_grapheme(Alph, G).

parse_subst_var(Alphs, P0) -->
    parse_subst_var0(Alphs, P),
    parse_subst_transforms(Alphs, P, P0).

parse_subst_var0(Alphs, P) -->
    `@`, !, id(Id),
    `(`,
      parse_grapheme_subst(Alphs, P0),
      `)`,
    { subst_func(Id, Alphs, P0, P) }.

parse_subst_var0(Alphs, P) -->
    `(`, !,
      parse_grapheme_subst(Alphs, P),
      `)`.

parse_subst_var0(_, 1-inf) --> `{}`, !.
parse_subst_var0(_, N-M) -->
    `{`, integer(N), { N > 0 },
      ( integer(M0), { M0 < 0, M is -M0, M >= N }, !;
        `-`, { M = inf }, !;
        [], { M = N }), `}`, !.
parse_subst_var0(_, -N) -->
    `{/`, integer(N), { N > 0 }, `}`, !.

parse_subst_transforms(Alphs, P, P1) -->
    parse_subst_transform(Alphs, P, P0), !,
    parse_subst_transforms(Alphs, P0, P1).

parse_subst_transforms(_, P, P) --> [].

parse_subst_transform(SAlph/_, P, remove(P, M)) -->
    `-`, modifier(SAlph, M).
parse_subst_transform(_/DAlph, P, add(P, M)) -->
    `+`, modifier(DAlph, M).

subst_func(upper, _, P, upper(P)) :- !.
subst_func(lower, _, P, lower(P)) :- !.
subst_func(chop, _, P, chop(P)) :- !.
subst_func(Id, Alphs, P, apply(Id, Alphs, P)).

parse_grapheme_subst(Alphs, P, X) :-
    phrase(parse_grapheme_subst(Alphs, P), X).

transform_graphemes(Fence, Rule, SAlph/DAlph, A, B) -->
    transform_rec_grapheme(Fence, Rule, SAlph), !,
    transform_graphemes(Fence, Rule, SAlph/DAlph, A, B).

transform_graphemes(Fence, Rule, SAlph/DAlph, A, B) -->
    { transform_rule(Rule, SAlph, P, DAlph, R, false) },
    grapheme_pattern(Fence, P, L),
    { grapheme_replace(R, L, X), !, appendv(X, A, B0),
      advance(Fence, L, [], Fence0) },
    transform_graphemes(Fence0, Rule, SAlph/DAlph, B0, B).

transform_graphemes(_, Rule, Alph/Alph, [G | A], B) -->
    [G], !,
    transform_graphemes(false, Rule, Alph/Alph, A, B).

transform_graphemes(_, _, _, H, H) --> [].

transform_rec_grapheme(Fence, Rule, SAlph), X -->
    { transform_rule(Rule, SAlph, P, SAlph, R, true) },
    grapheme_pattern(Fence, P, L),
    { grapheme_replace(R, L, X) }.

transform_graphemes(Rule, SAlph/DAlph, X, Y) :-
    phrase(transform_graphemes(true, Rule, SAlph/DAlph, Y, []), X),
    maplist(valid_grapheme(DAlph), Y).

enum_transforms(Id, SAlph/DAlph, From = To) :-
    transform_rule(Id, SAlph, P, DAlph, R, false),
    enum_graphemes(SAlph, P, From),
    grapheme_replace(R, From, To).

transform_target(Id, SAlph, DAlph) :-
    transform_rule(Id, SAlph, _, DAlph, _, false), !.
