:- module(config, [load_grammar_file/1]).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(alphabet).
:- use_module(utils).

load_grammar_file(File) -->
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

alphabet_defs(_) --> blanks, `;`, !.

alphabet_defs(Alph) -->
    alphabet_def(Alph), !,
    alphabet_defs(Alph).

alphabet_def(_) --> comment, !.
alphabet_def(Alph) -->
    `letters`, letters(Alph), blanks, `;`, !.

alphabet_def(Alph) -->
    `modifiers`, modifiers(Alph), blanks, `;`, !.

alphabet_def(Alph) -->
    `class`, blanks1, id(Name), !,
    list_of_graphemes(LG), blanks, `;`,
    { define_letter_class(Alph, Name, LG) }.

alphabet_def(_) --> syntax_error('Unknown alphabet definition').

letters(Alph) --> letter(Alph), !, letters(Alph).
letters(_) --> [].

letter(Alph) --> blanks1, uchars(L), { define_base_letter(Alph, L) }.

modifiers(Alph) --> modifier(Alph), !, modifiers(Alph).
modifiers(_) --> [].

modifier(Alph) --> blanks1, uchars(L), { define_modifier(Alph, L) }.

uchars(L) --> `'`, !, string_without(`'`, L0), `'',
    uchars(T),
    { append(L0, T, L) }.
uchars([C | T]) --> uchar(C), !, uchars(T).
uchars([]) --> [].

uchar(C) --> `\\`, !, [C].
uchar(C) --> `<U+`, 

comment -->
    blanks, `#`, !,
    string_without(`\n`, _),
    `\n`.

blanks1 --> blank, blanks.
