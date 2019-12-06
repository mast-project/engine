:- module(pipeline, [define_pipeline/3, run_pipeline/4]).
:- use_module(library(error)).
:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(alphabet).

:- dynamic pipeline/3.
:- multifile pipeline_type/3, pipeline_step/2.

pipeline_typecheck([], InType, InType, []).
pipeline_typecheck([H | T], InType, OutType, [H : (InType -> OutType0) | TT]) :-
    pipeline_type(H, InType, OutType0),
    pipeline_typecheck(T, OutType0, OutType, TT).

define_pipeline(Name, InType, Steps) :-
    forall(pipeline_typecheck(Steps, InType, OutType, TypedSteps),
           assertz(pipeline(Name, (InType -> OutType), TypedSteps))), !.

define_pipeline(Name, _, _) :-
    domain_error(pipeline, Name).

run_pipeline(Name, InType -> OutType, Input, Output) :-
    pipeline(Name, InType -> OutType, Steps),
    run_pipeline_steps(Steps, Input, Output), !.

run_pipeline_steps([], Input, Input).
run_pipeline_steps([H : Types | T], Input, Output) :-
    pipeline_step(H, Types, Input, Output0),
    run_pipeline_steps(T, Output0, Output).

pipeline_type(read, filename, [uchars]).
pipeline_type(join, [uchars], [uchars]).
pipeline_type(join, [[X]], [X]).
pipeline_type(graphemes(Alph), uchars, [grapheme(Alph)]).
pipeline_type(map(Step), [InType], [OutType]) :-
    pipeline_type(Step, InType, OutType).

pipeline_type(call(Id), InType, OutType) :-
    pipeline(Id, (InType -> OutType), _).

pipeline_type(transform(Id), [grapheme(SrcAlph)], [grapheme(DstAlph)]) :-
    transform_target(Id, SrcAlph, DstAlph).

pipeline_type(dump, X, X).

pipeline_step(read, (filename -> [uchars]), FileName, Lines) :-
    setup_call_cleanup(open(FileName, read, Stream),
                       read_lines(Stream, Lines),
                       close(Stream)).

pipeline_step(join, _, X, Y) :-
    append(X, Y).

pipeline_step(graphemes(Alph), _, Chars, Graphemes) :-
    phrase(graphemes(Alph, Graphemes), Chars).

pipeline_step(map(Step),  ([InType] -> [OutType]), Input, Output) :-
    maplist(pipeline_step(Step, (InType -> OutType), Input, Output)).

pipeline_step(call(Id), Types, Input, Output) :-
    run_pipeline(Id, Types, Input, Output).

pipeline_step(transform(Id), ([grapheme(SrcAlph)] -> [grapheme(DstAlph)]),
              Input, Output) :-
    transform_graphemes(Id, SrcAlph/DstAlph, Input, Output).

pipeline_step(dump, ([grapheme(Alph)] -> _), Input, Input) :-
    graphemes_string(Alph, Input, L),
    maplist(put_code, L).

read_lines(Stream, [H | T]) :-
    read_line_to_codes(Stream, H, []),
    H \= [], !,
    read_lines(Stream, T).
