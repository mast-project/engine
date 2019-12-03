:- module(syntax, []).
:- use_module(library(chr)).

:- chr_constraint constituent/6.

constituent(Min1, Max1, Pos1, M1, Sub1, _),
constituent(Min2, Max2, Pos2, M2, Sub2, true) |
government(M1, M2, Ms1, Ms2), Min2 = Max1 + 1 ==>
constituent(Min1, Max2, Pos1, Ms1,
           [constituent(Min2, Max2, Pos2, Ms2, Sub2, true) | Sub1]
