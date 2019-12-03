:- module(morpho, [parse_form/3, parse_form_cached/3,
                  make_alteration/2, define_rule/4, define_structure/1]).
:- use_module(alphabet).

:- dynamic alteration/3, rule/4, structure/1, mcache/3.

alteration('', [], []).

apply_rule(Form, M, Patterns, MR, Stem, NewM) :-
    M >:< MR,
    apply_patterns(Form, Patterns, Stem),
    put_dict(MR, M, NewM).

apply_patterns(Form, [], Form).
apply_patterns(Form, [H|T], Stem) :-
    apply_pattern(Form, H, Stem0),
    apply_patterns(Stem0, T, Stem).

apply_pattern(Form, word(P), Form) :-
    grapheme_match(P, Form).
apply_pattern(Form, suppl(P, NewForm), Form) :-
    grapheme_match(P, Form).
apply_pattern(Form, prefix(P, LA, Alts), Stem) :-
    grapheme_patsplit(P, Form, _, Stem0),
    grapheme_patsplit(LA, Stem0, _, _),
    prefix_alteration(Stem0, Alts, Stem).
apply_pattern(Form, suffix(P, LB, Alts), Stem) :-
    grapheme_patsuffix(P, Form, Stem0, _),
    grapheme_patsuffix(LB, Stem0, _, _),
    suffix_alteration(Stem0, Alts, Stem).
apply_pattern(Form, infix(P, LB, AltsB, LA, AltsA), Stem) :-
    grapheme_patsubstr(P, Form, Before, _, After),
    grapheme_patsuffix(LB, Before, _, _),
    grapheme_patsplit(LA, After, _, _),
    suffix_alteration(Before, AltsB, Before0),
    prefix_alteration(After, AltsA, After0),
    append(Before0, After0, Stem).

prefix_alteration(Form, Alts, Stem) :-
    member(Alt, Alts),
    alteration(Alt, From, To),
    append(To, Form0, Form),
    append(From, Form0, Stem).

suffix_alteration(Form, Alts, Stem) :-
    member(Alt, Alts),
    alteration(Alt, From, To),
    append(Form0, To, Form),
    append(Form0, From, Stem).

select_rule(Step, Form, M, Stem, NewM) :-
    rule(Step, Patterns, MR, Exclusive),
    apply_rule(Form, M, Patterns, MR, Stem, NewM),
    (Exclusive = true -> !; true).

parse_step(?(Step), Form, M, Form0, M0) :-
    parse_step(Step, Form, M, Form0, M0).
parse_step(?(_), Form, M, Form, M) :- !.
parse_step(Step, Form, M, Form0, M0) :-
    select_rule(Step, Form, M, Form0, M0).

parse_structure(Form, M, [], Form, M).
parse_structure(Form, M, [Step | T], Stem, M1) :-
    parse_step(Step, Form, M, Form0, M0),
    parse_structure(Form0, M0, T, Stem, M1).

parse_form(Form, Stem, M) :-
    structure(S),
    parse_structure(Form, features{}, S, Stem, M).

parse_form_cached(Form, Stem, M) :-
    mcache(Form, Stem, M).
parse_form_cached(Form, Stem, M) :-
    \+ mcache(Form, _, _),
    forall(parse_form(Form, Stem0, M0),
           assertz(mcache(Form, Stem0, M0))),
    mcache(Form, Stem, M).

make_alteration(Name, _) :-
    alteration(Name, _, _), !.
make_alteration(Name, Alph) :-
    forall(enum_transforms(Name, Alph/Alph, From = To),
           assertz(alteration(Name, From, To))).

define_rule(Name, Pats, M, Excl) :-
    assertz(rule(Name, Pats, M, Excl)).

define_structure(S) :-
    assertz(structure(S)).
