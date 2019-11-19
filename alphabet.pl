:- module(alphabet, []).

:- dynamic base_letter/2, compound_grapheme/2, modifier/2.

simple_grapheme(Alph, [C | T]) -->
    [C],
    { base_letter(Alph, C) },
    diacritics(Alph, T).

diacritics(Alph, [C | T]) -->
    [C],
    { modifier(Alph, C) }, !,
    diacritics(Alph, T).

diacritics(_, []) --> [].

grapheme(Alph, L) -->
    simple_graphemes(Alpha, LS),
    { base_letters(LS, BL), compound_grapheme(
