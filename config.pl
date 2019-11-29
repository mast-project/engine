 :- module(config, [load_grammar_file/1]).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(alphabet).
:- use_module(utils).

load_grammar_file(File) :-
    phrase_from_file(grammar_file, File).

grammar_file --> blanks, eos, !.

grammar_file -->
    grammar_statement, !,
    grammar_file.

grammar_file --> comment, !.

grammar_statement -->
    blanks, `include`, blanks1, !,
    nonblanks(L),
    { L \= [], atom_codes(File, L) },
    blanks, `;`,
    { load_grammar_file(File) }.

grammar_statement -->
    blanks, `alphabet`, blanks1, !,
    id(Alph),
    alphabet_defs(Alph).

grammar_statement --> syntax_error('Unknown statement').

alphabet_defs(Alph) -->
    alphabet_def(Alph), !,
    blanks,
    (`;`, !;
     `,`, alphabet_defs(Alph)).

alphabet_def(_) --> comment, !.
alphabet_def(Alph) -->
    blanks, `letters`, letters(Alph), !.

alphabet_def(Alph) -->
    blanks, `modifiers`, modifiers(Alph), !.

alphabet_def(Alph) -->
    blanks, `class`, blanks1, id(Name), !,
    list_of_graphemes(Alph, LG),
    { define_letter_class(Alph, Name, LG) }.

alphabet_def(Alph) -->
    blanks, `token`, blanks1, id(Name), !,
    blanks1, uchars(Prefix),
    blanks1, list_of_classes(Classes),
    blanks1, uchars(Suffix), blanks, `;`,
    { define_text_token(Alph, Name, Prefix, Classes, Suffix) }.

alphabet_def(_) --> syntax_error('Unknown alphabet definition').

letters(Alph) --> letter(Alph), !, letters(Alph).
letters(_) --> [].

letter(Alph) --> blanks1, uchars(L), { define_base_letter(Alph, L) }.

modifiers(Alph) --> modifier(Alph), !, modifiers(Alph).
modifiers(_) --> [].

modifier(Alph) --> blanks1, uchars(L), { define_modifier(Alph, L) }.

uchars(L) --> `'`, !, string_without(`'`, L0), `'`, %'
    uchars0(T),
    { append(L0, T, L) }.
uchars([C | T]) --> uchar(C), !, uchars0(T).

uchars0(L) --> uchars(L), !.
uchars0([]) --> [].

uchar(C) --> `\\`, !, [C].
uchar(C) --> `<U+`, !, xinteger(C), `>`.
uchar(C) --> [C], { C \== 0';, \+ code_type(C, space) }.

list_of_graphemes(Alph, [G | T]) -->
    blanks1,
    parse_single_grapheme(Alph, G),
    list_of_graphemes(Alph, T), !.
list_of_graphemes(_, []) --> blanks, `;`.

list_of_classes([C|T]) -->
    id(C), blanks, `|`, blanks, !,
    list_of_classes(T).
list_of_classes([C]) -->
    id(C), !.
list_of_classes([]) --> [].

comment -->
    blanks, `#`, !,
    string_without(`\n`, _),
    `\n`.

blanks1 --> blank, blanks.
