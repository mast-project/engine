 :- module(config, [load_grammar_file/1]).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(alphabet).
:- use_module(utils).

load_grammar_file(File) :-
    phrase_from_file(grammar_file, File).

grammar_file --> gblanks, eos, !.

grammar_file -->
    grammar_statement, !,
    grammar_file.

grammar_statement -->
    gblanks, `include`, gblanks1, !,
    nonblanks(L),
    { L \= [], atom_codes(File, L) },
    gblanks, `;`,
    { load_grammar_file(File) }.

grammar_statement -->
    gblanks, `alphabet`, gblanks1, !,
    id(Alph),
    alphabet_defs(Alph).

grammar_statement -->
    gblanks, `transform`, gblanks1, !,
    id(Rule), gblanks, `:`, gblanks, id(SAlph), gblanks, `->`, gblanks, id(DAlph),
    transform_defs(Rule, SAlph, DAlph),
    gblanks, expect(`;`).

grammar_statement --> syntax_error(unknown_statement).

alphabet_defs(Alph) -->
    alphabet_def(Alph), !,
    gblanks,
    (`;`, !;
     expect(`,`), alphabet_defs(Alph)).

alphabet_def(Alph) -->
    gblanks, `letters`, letters(Alph), !.

alphabet_def(Alph) -->
    gblanks, `modifiers`, modifiers(Alph), !.

alphabet_def(Alph) -->
    gblanks, `class`, gblanks1, id(Name), !,
    list_of_graphemes(Alph, LG),
    { define_letter_class(Alph, Name, LG) }.

alphabet_def(Alph) -->
    gblanks, `token`, gblanks1, id(Name), !,
    gblanks1, uchars(Prefix),
    gblanks1, list_of_classes(Classes),
    gblanks1, uchars(Suffix),
    { define_text_token(Alph, Name, Prefix, Classes, Suffix) }.

alphabet_def(_) --> syntax_error(unknown_alphabet_def).

transform_defs(Id, SAlph, DAlph) -->
    transform_def(Id, SAlph, DAlph), !,
    transform_defs(Id, SAlph, DAlph).

transform_defs(_, _, _) --> [].

transform_def(Id, SAlph, DAlph) -->
    gblanks1, parse_grapheme_pattern(SAlph, Pat),
    gblanks1, arrow(SAlph, DAlph, RAlph, Rec),
    gblanks1, parse_grapheme_subst(SAlph/RAlph, R),
    { def_transform(Id, SAlph, RAlph, Pat, R, Rec) }.

arrow(Alph, _, Alph, true) --> `->*`, !.
arrow(_, Alph, Alph, false) --> `->`.

def_transform(Id, SAlph, DAlph, P, R, false) :-
    define_transform_rule(Id, SAlph, P, DAlph, R).

def_transform(Id, SAlph, SAlph, P, R, true) :-
    define_rec_transform_rule(Id, SAlph, P, R).

letters(Alph) --> letter(Alph), !, letters(Alph).
letters(_) --> [].

letter(Alph) --> gblanks1, uchars(L), { define_base_letter(Alph, L) }.

modifiers(Alph) --> modifier(Alph), !, modifiers(Alph).
modifiers(_) --> [].

modifier(Alph) --> gblanks1, uchars(L), { define_modifier(Alph, L) }.

uchars(L) --> `'`, !, string_without(`'`, L0), `'`, %'
    uchars0(T),
    { append(L0, T, L) }.
uchars([C | T]) --> uchar(C), !, uchars0(T).

uchars0(L) --> uchars(L), !.
uchars0([]) --> [].

uchar(C) --> `\\`, !, [C].
uchar(C) --> `<U+`, !, xinteger(C), `>`.
uchar(C) --> \+ separator, [C].

list_of_graphemes(Alph, [G | T]) -->
    gblanks1,
    parse_single_grapheme(Alph, G),
    list_of_graphemes(Alph, T), !.
list_of_graphemes(_, []) --> [].

list_of_classes([C|T]) -->
    id(C), gblanks, `|`, gblanks, !,
    list_of_classes(T).
list_of_classes([C]) -->
    id(C), !.
list_of_classes([]) --> [].

gblanks1 --> gblank, gblanks.

gblanks --> gblank, !, gblanks.
gblanks --> [].

gblank --> comment, !.
gblank --> blank.

comment -->
    `%`,
    string_without(`\n`, _),
    `\n`.
