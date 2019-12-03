:- module(syntax, [constituent/3]).
:- use_module(library(chr)).

:- chr_constraint constituent/3.

constituent(Pos1, M1, Sub1), constituent(Pos2, M2, Sub2)  <=>
Pos1 \= Pos2, government(M1, M2, Master, Slave),
is_complete(Slave, Sub2) |
constituent(Pos1, [Master], [constituent(Pos2, [Slave], Sub2, true) | Sub1]).

constituent(Pos, M, Sub), constituent(Pos, M, Sub) <=> constituent(Pos, M, Sub).

constituent(Pos, _, _), constituent(Pos, _, _) <=> fail.

government(M1, M2, Master, Slave) :-
    member(Master, M1),
    member(Slave, M2),
    rule(Master, Slave).

is_complete(Master, Sub) :-
    forall(required(Master, Slave),
           memberchk(constituent(_, [Slave], _, _), Sub)), !.

rule(prep, noun).
rule(verb, noun).
rule(noun, adj).

required(prep, noun).