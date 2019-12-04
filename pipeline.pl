:- module(pipeline, []).

:- dynamic pipeline/2.

pipeline_step(transform, alphabet, grapheme, grapheme).
pipeline_step(split, grapheme, grapheme, [grapheme]).
pipeline_step(split, grapheme, [grapheme], [[grapheme]]).
