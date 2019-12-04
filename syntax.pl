:- module(syntax, [constituent/3]).
:- use_module(library(chr)).

:- chr_constraint constituent/3.

constituent(Min1-Pos1-Max1, M1, Sub1), constituent(Min2-Pos2-Max2, M2, Sub2) ==>
                                       Max1 + 1 =:=  Min2, rule(M1, M2),
is_complete(M2, Sub2) |
constituent(Min1-Pos1-Max2, M1, [constituent(Min2-Pos2-Max2, M2, Sub2) | Sub1]).

constituent(Min1-Pos1-Max1, M1, Sub1), constituent(Min2-Pos2-Max2, M2, Sub2) ==>
                                       Max2 + 1 =:=  Min1, rule(M1, M2),
is_complete(M2, Sub2) |
constituent(Min2-Pos1-Max1, M1, [constituent(Min2-Pos2-Max2, M2, Sub2) | Sub1]).

is_complete(Master, Sub) :-
    forall(required(Master, Slave),
           memberchk(constituent(_, Slave, _, _), Sub)), !.

rule(prep, noun).
rule(verb, noun).
rule(noun, adj).

required(prep, noun).
