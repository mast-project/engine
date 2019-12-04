 :- module(config, [load_grammar_file/1]).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(alphabet).
:- use_module(morpho).
:- use_module(utils).

:- dynamic default_alphabet/1, category/2, label/2.

load_grammar_file(File) :-
    phrase_from_file(grammar_file, File).

grammar_file --> gblanks, grammar_file0.

grammar_file0 --> eos, !.

grammar_file0 -->
    grammar_statement, !,
    grammar_file0.

grammar_statement -->
    keyword(`include`), !,
    nonblanks(L),
    { L \= [], atom_codes(File, L) },
    gblanks, expect_lexeme(`;`),
    { load_grammar_file(File) }.

grammar_statement -->
    is_default(Default), keyword(`alphabet`), !,
    lexeme(id(Alph)),
    alphabet_defs(Alph),
    { Default = true -> asserta(default_alphabet(Alph)); true }.

grammar_statement -->
    keyword(`transform`), !,
    lexeme(id(Rule)), lexeme(`:`),
    lexeme(id(SAlph)), lexeme(`->`),
    lexeme(id(DAlph)),
    transform_defs(Rule, SAlph, DAlph),
    expect_lexeme(`;`).

grammar_statement -->
    keyword(`category`), !,
    lexeme(id(Name)), lexeme(`:`),
    ids(Labels),
    expect_lexeme(`;`),
    { assertz(category(Name, Labels)) }.

grammar_statement -->
    lexeme(id(Name)), lexeme(`=`), !,
    labeldef(Def), expect_lexeme(`;`),
    { assertz(label(Name, Def)) }.

grammar_statement -->
    keyword(`word`), !,
    structure_def(S),
    expect_lexeme(`;`),
    { define_structure(S) }.

grammar_statement -->
    is_exclusive(Excl), keyword(`paradigm`), !,
    lexeme(id(Label)), check_label(Label),
    altfeatures(IFs),
    { findall(F, expand_label(Label, F), Fs) },
    paradigm_items(Label, IFs, Fs, Excl),
    expect_lexeme(`;`).

grammar_statement --> syntax_error(unknown_statement).

alphabet_defs(Alph) -->
    alphabet_def(Alph), !,
    (lexeme(`;`), !;
     expect_lexeme(`,`), alphabet_defs(Alph)).

alphabet_def(Alph) -->
    keyword(`letters`), letters(Alph), !.

alphabet_def(Alph) -->
    keyword(`modifiers`), modifiers(Alph), !.

alphabet_def(Alph) -->
    keyword(`class`), lexeme(id(Name)), !,
    list_of_graphemes(Alph, LG),
    { define_letter_class(Alph, Name, LG) }.

alphabet_def(Alph) -->
    keyword(`token`), lexeme(id(Name)), !,
    uchars(Prefix),
    list_of_classes(Classes),
    uchars(Suffix),
    { define_text_token(Alph, Name, Prefix, Classes, Suffix) }.

alphabet_def(_) --> syntax_error(unknown_alphabet_def).

transform_defs(Id, SAlph, DAlph) -->
    transform_def(Id, SAlph, DAlph), !,
    transform_defs(Id, SAlph, DAlph).

transform_defs(_, _, _) --> [].

transform_def(Id, SAlph, DAlph) -->
    lexeme(parse_grapheme_pattern(SAlph, Pat)),
    arrow(SAlph, DAlph, RAlph, Rec),
    lexeme(parse_grapheme_subst(SAlph/RAlph, R)),
    { def_transform(Id, SAlph, RAlph, Pat, R, Rec) }.

arrow(Alph, _, Alph, true) --> lexeme(`->*`), !.
arrow(_, Alph, Alph, false) --> lexeme(`->`).

def_transform(Id, SAlph, DAlph, P, R, false) :-
    define_transform_rule(Id, SAlph, P, DAlph, R).

def_transform(Id, SAlph, SAlph, P, R, true) :-
    define_rec_transform_rule(Id, SAlph, P, R).

structure_def(S) -->
    `(`, idlabel(Root), `)`, !,
    struct_suffixes([Root], S).

structure_def([?(Label) | T]) -->
    `[`, idlabel(Label), `-]`, !,
    structure_def(T).

structure_def([Label | T]) -->
    idlabel(Label), `-`,
    structure_def(T).

struct_suffixes(T, S) -->
    `[-`, idlabel(Label), `]`, !,
    struct_suffixes([?(Label) | T], S).

struct_suffixes(T, S) -->
    `-`, idlabel(Label), !,
    struct_suffixes([Label | T], S).

struct_suffixes(T, T) --> gblanks.

labeldef(Fs) --> altfeatures(Fs), !.
labeldef(fusion(C1, C2, Exc)) -->
    lexeme(id(C1)),
    lexeme(`*`), !,
    lexeme(id(C2)),
    exceptions(Exc),
    check_category(C1),
    check_category(C2),
    check_values(C1, Exc).

labeldef(Label) --> lexeme(id(Label)), !.

exceptions(Exc) --> lexeme(`\\`), !, ids(Exc).
exceptions([]) --> [].

paradigm_items(_, _, [], _) --> [].
paradigm_items(Label, IFS, [F | Fs], Excl) -->
    paradigm_item(Label, IFS, F, Excl),
    paradigm_items(Label, IFS, Fs, Excl).

paradigm_item(_, _, _, _) --> lexeme(`---`), !.
paradigm_item(Label, IFs, F, Excl) -->
    form_patterns(Pats),
    { forall((member(IF, IFs),
              member(P, Pats)),
             (put_dict(F, IF, M),
              define_rule(Label, P, M, Excl))) }.

form_patterns([P | Ps]) -->
    form_pattern(P), lexeme(`,`), !,
    form_patterns(Ps).
form_patterns([P]) --> form_pattern(P).

form_pattern([prefix(Pfx, LA, Alts) | P]) -->
    form_pattern_prefix(Pfx, LA, Alts), !,
    form_pattern1(P).
form_pattern(P) --> form_pattern1(P), !.
form_pattern([suppl(P, F)]) -->
    { default_alphabet(Alph) },
    lexeme(parse_grapheme_simple_pattern(Alph, P)),
    lexeme(`=`), !,
    lexeme(grapheme_word(Alph, F)).
form_pattern([word(P)]) -->
    { default_alphabet(Alph) },
    lexeme(parse_grapheme_simple_pattern(Alph, P)).

form_pattern1([infix(Inf, LB, AltsB, LA, AltsA) | P]) -->
    form_pattern_infix(Inf, LB, AltsB, LA, AltsA), !,
    (form_pattern1(P); { P = [] }).
form_pattern1([suffix(Sfx, LB, Alts)]) -->
    form_pattern_suffix(Sfx, LB, Alts).

form_pattern_prefix(Pfx, LA, Alts) -->
    { default_alphabet(Alph) },
    parse_grapheme_simple_pattern(Alph, Pfx),
    context_or_alt(Alph, LA, Alts),
    `-`.

form_pattern_suffix(Sfx, LB, Alts) -->
    { default_alphabet(Alph) }, `-`,
    context_or_alt(Alph, LB, Alts),
    parse_grapheme_simple_pattern(Alph, Sfx).

form_pattern_infix(Infx, LB, AltsB, LA, AltsA) -->
    { default_alphabet(Alph) }, `-`,
    context_or_alt(Alph, LB, AltsB),
    parse_grapheme_simple_pattern(Alph, Infx),
    context_or_alt(Alph, LA, AltsA),
    `-`.

context_or_alt(Alph, L, []) -->
   `(`, !, parse_grapheme_simple_pattern(Alph, L), `)`.
context_or_alt(Alph, null, ['' | Alts]) -->
   `<[`, !, alterations(Alph, Alts), `]>`.
context_or_alt(Alph, null, Alts) -->
   `<`, !, alterations(Alph, Alts), `>`.
context_or_alt(_, null, []) --> [].

alterations(Alph, [A | Alts]) -->
    id(A), { make_alteration(Alph, A) }, `|`, !,
    alterations(Alph, Alts).

alterations(Alph, [A]) -->
    id(A), { make_alteration(Alph, A) }.

expand_label(Label, F) :-
    label(Label, Def), !,
    expand_label_def(Def, F).
expand_label(Label, F) :-
    category(Label, Vs),
    member(V, Vs),
    dict_create(F, labels, [Label = V]).

expand_label_def(Label, F) :-
    atom(Label), !,
    expand_label(Label, F).

expand_label_def(Fs, F) :-
    is_list(Fs), !,
    member(F, Fs).

expand_label_def(fusion(C1, C2, Excl), F) :-
    category(C1, Vals1),
    category(C2, Vals2),
    member(V1, Vals1),
    (memberchk(V1, Excl) -> V2 = _; member(V2, Vals2)),
    dict_create(F, labels, [C1 = V1; C2 = V2]).

idlabel(Label) --> id(Label), check_label(Label).

check_label(Label) --> { label(Label, _)}, !.
check_label(Label) --> { category(Label, _)}, !.
check_label(Label) --> syntax_error(undefined_label(Label)).

check_category(Label) --> { category(Label, _)}, !.
check_category(Label) --> syntax_error(undefined_category(Label)).

check_values(Cat, L) --> { category(Cat, All), subset(L, All) }, !.
check_values(Cat, L) --> syntax_error(invalid_values(Cat, L)).

letters(Alph) --> letter(Alph), !, letters(Alph).
letters(_) --> [].

letter(Alph) --> uchars(L), { define_base_letter(Alph, L) }.

modifiers(Alph) --> modifier(Alph), !, modifiers(Alph).
modifiers(_) --> [].

modifier(Alph) --> uchars(L), { define_modifier(Alph, L) }.

uchars(L) --> `'`, !, string_without(`'`, L0), `'`, %'
    uchars0(T),
    { append(L0, T, L) }.
uchars([C | T]) --> uchar(C), !, uchars0(T).

uchars0(L) --> uchars(L), !.
uchars0([]) --> gblanks.

uchar(C0) --> `<C-, [C], { code_type(C, lower), C0 is C - 60 }, `>`, !.
uchar(C) --> `\\`, !, [C].
uchar(C) --> `<U+`, !, xinteger(C), `>`.
uchar(C) --> \+ separator, [C].

list_of_graphemes(Alph, [G | T]) -->
    lexeme(parse_single_grapheme(Alph, G)),
    list_of_graphemes(Alph, T), !.
list_of_graphemes(_, []) --> gblanks.

list_of_classes([C|T]) -->
    lexeme(id(C)), lexeme(`|`), !,
    list_of_classes(T).
list_of_classes([C]) -->
    lexeme(id(C)), !.
list_of_classes([]) --> [].

ids([H|T]) -->
    lexeme(id(H)), !,
    ids(T).
ids([]) --> [].

altfeatures([H|T]) -->
    features(H), lexeme(`|`), !,
    altfeatures(T).
altfeatures([H]) --> features(H).

features(F) -->
    lexeme(`{`),
    catvalues(KV),
    lexeme(`}`),
    { dict_create(F, features, KV) }.

catvalues([C = V | T]) -->
    lexeme(id(C)), lexeme(`:`),
    lexeme(id(V)),
    check_cv(C, V), !,
    catvalues(T).

catvalues([]) --> [].

check_cv(C, V) -->
    { category(C, VL),
      memberchk(V, VL) }, !.
check_cv(C, V) -->
    syntax_error(invalid_category(C, V)).

is_default(true) --> keyword(`default`), !.
is_default(false) --> [].

is_exclusive(true) --> keyword(`exclusive`), !.
is_exclusive(false) --> [].

keyword(K) --> K, gblanks1.

lexeme(G) --> G, gblanks.

expect_lexeme(L) --> expect(L), gblanks.

gblanks1 --> gblank, gblanks.

gblanks --> gblank, !, gblanks.
gblanks --> [].

gblank --> comment, !.
gblank --> blank.

comment -->
    `%`,
    string_without(`\n`, _),
    `\n`.
