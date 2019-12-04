:- module(syntax, [reduce_syntax/2]).

select_two(H1, H2, [H1, H2 | T], T) --> [].
select_two(H1, H2, [H | T], T0) --> [H],
    select_two(H1, H2, T, T0).

select_two(H1, H2, Src, Pfx, Sfx) :-
    phrase(select_two(H1, H2, Src, Sfx), Pfx).

reduce_syntax([X], [X]) :- !.
reduce_syntax(Src, Dst) :-
    select_two(X, Y, Src, Pfx, Sfx),
    reduce(X, Y, Z),
    append(Pfx, [Z | Sfx], Src0),
    reduce_syntax(Src0, Dst).

reduce(X, Y, Z) :-
    govern(X, Y, Z).
reduce(X, Y, Z) :-
    govern(Y, X, Z).

govern(constituent(Pos1, M1, Sub1), constituent(Pos2, M2, Sub2),
       constituent(Pos1, M1, [constituent(Pos2, M2, Sub2) | Sub1])) :-
    rule(M1, M2),
    is_complete(M2, Sub2).

is_complete(Master, Sub) :-
    forall(required(Master, Slave),
           memberchk(constituent(_, Slave, _, _), Sub)), !.

rule(prep, noun).
rule(verb, noun).
rule(noun, adj).

required(prep, noun).
