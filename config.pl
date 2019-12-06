 :- module(config, [load_grammar_file/1]).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(alphabet).
:- use_module(morpho).
:- use_module(pipeline).
:- use_module(utils).

:- dynamic default_alphabet/1, category/2, label/2.

:- multifile parse_pipeline_step//1.

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
    labeldef(Add),
    { findall(F, expand_label_def(Label * Add, F), Fs) },
    paradigm_items(_, Label, Fs, Excl),
    expect_lexeme(`;`).

grammar_statement -->
    keyword(`pipeline`), !,
    lexeme(id(Name)), lexeme(`:`), pipeline_type(Type),
    parse_pipeline_steps(Steps), expect_lexeme(`;`),
    { define_pipeline(Name, Type, Steps) }.

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
    uchars0(Suffix),
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

labeldef((L1; L2)) -->
    labeldef_alt(L1), lexeme(`|`), !,
    labeldef(L2).

labeldef(L) -->
    labeldef_alt(L).

labeldef_alt(L1 * L2) -->
    labeldef_prod(L1), lexeme(`*`), !,
    labeldef_alt(L2).

labeldef_alt(L) -->
    labeldef_prod(L).

labeldef_prod(L1 - L2) -->
    labeldef_atomic(L1), lexeme(`\\`), !,
    labeldef_prod(L2).

labeldef_prod(L) -->
    labeldef_atomic(L).

labeldef_atomic(Id) -->
    lexeme(id(Id)), !,
    check_label(Id).

labeldef_atomic(F) -->
    features(F), !.

labeldef_atom(X) --> `(`, labeldef(X), `)`.

paradigm_items(_, _, [], _) --> [].
paradigm_items(PrevPats, Label, [F | Fs], Excl) -->
    paradigm_item(PrevPats, Label, F, Excl, Pats),
    paradigm_items(Pats, Label, Fs, Excl).

paradigm_item(_, _, _, _, _) --> lexeme(`---`), !.
paradigm_item(PrevPats, Label, F, Excl, PrevPats) -->
    lexeme(`<<<`),
    { nonvar(PrevPats),
      forall(member(P, PrevPats),
             define_rule(Label, P, F, Excl)) }.

paradigm_item(_, Label, F, Excl, Pats) -->
    form_patterns(Pats),
    { forall(member(P, Pats),
             define_rule(Label, P, F, Excl)) }.

form_patterns([P | Ps]) -->
    form_pattern(P), lexeme(`,`), !,
    form_patterns(Ps).
form_patterns([P]) --> form_pattern(P), !.

form_pattern([prefix(Pfx, LA, Alts) | P]) -->
    form_pattern_prefix(Pfx, LA, Alts), !,
    form_pattern1(P),
    gblanks.
form_pattern(P) -->
    form_pattern1(P),
    gblanks, !.
form_pattern([suppl(P, F)]) -->
    { default_alphabet(Alph) },
    lexeme(parse_grapheme_simple_pattern(Alph, P)),
    lexeme(`=`), !,
    lexeme(grapheme_word(Alph, F)).
form_pattern([word(P)]) -->
    { default_alphabet(Alph) },
    lexeme(parse_grapheme_simple_pattern(Alph, P)), !.

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

context_or_alt(Alph, L, ['']) -->
   `(`, !, parse_grapheme_simple_pattern(Alph, L), `)`.
context_or_alt(Alph, null, ['' | Alts]) -->
   `<[`, !, alterations(Alph, Alts), `]>`.
context_or_alt(Alph, null, Alts) -->
   `<`, !, alterations(Alph, Alts), `>`.
context_or_alt(_, null, ['']) --> [].

alterations(Alph, [A | Alts]) -->
    id(A), { make_alteration(Alph, A) }, `|`, !,
    alterations(Alph, Alts).

alterations(Alph, [A]) -->
    id(A), { make_alteration(Alph, A) }.

parse_pipeline_steps([H | T]) -->
    lexeme(`->`), parse_pipeline_step(H), !,
    parse_pipeline_steps(T).

parse_pipeline_steps([]) --> [].

parse_pipeline_step(read) --> keyword(`read`), !.
parse_pipeline_step(join) --> keyword(`join`), !.
parse_pipeline_step(dump) --> keyword(`dump`), !.
parse_pipeline_step(graphemes(Alph)) -->
    keyword(`graphemes`),
    lexeme(id(Alph)).
parse_pipeline_step(map(Step)) -->
    parse_pipeline_step(Step), !.
parse_pipeline_step(call(Id)) -->
    keyword(`call`),
    lexeme(id(Id)), !.
parse_pipeline_step(transform(Id)) -->
    keyword(`transform`),
    lexeme(id(Id)), !.

pipeline_type([]) --> keyword(`void`), !.
pipeline_type(Type) --> lexeme(id(Type)).

expand_label(Label, F) :-
    label(Label, Def), !,
    expand_label_def(Def, F).
expand_label(Label, F) :-
    category(Label, Vs),
    member(V, Vs),
    dict_create(F, features, [Label = V]).

expand_label_def(Label, F) :-
    atom(Label), !,
    expand_label(Label, F).

expand_label_def(F, F) :-
    is_dict(F, features), !.

expand_label_def(L1 - L2, F) :-
    expand_label_def(L1, F),
    \+ (expand_label_def(L2, NF), NF :< F).

expand_label_def((L1; L2), F) :-
    expand_label_def(L1, F);
    expand_label_def(L2, F).

expand_label_def(L1 * L2, F) :-
    expand_label_def(L1, F1),
    expand_label_def(L2, F2),
    F1 >:< F2,
    put_dict(F1, F2, F).

idlabel(Label) --> id(Label), check_label(Label).

check_label(Label) --> { label(Label, _)}, !.
check_label(Label) --> { category(Label, _)}, !.
check_label(Label) --> syntax_error(undefined_label(Label)).

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

uchar(C0) --> `<C-`, [C], { code_type(C, lower), C0 is C - 60 }, `>`, !.
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

features(F) -->
    lexeme(`{`),
    catvalues(KV),
    lexeme(`}`),
    { dict_create(F, features, KV) }.

catvalues([C = V | T]) -->
    lexeme(id(C)), lexeme(`:`),
    catval(C, V), !,
    catvalues(T).

catvalues([]) --> [].

catval(_, _V) --> lexeme(`?`), !.
catval(C, V) -->
    lexeme(id(V)),
    check_cv(C, V).

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
