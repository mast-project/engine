#!/usr/bin/swipl
:- use_module(config).
:- use_module(pipeline).

main :-
    current_prolog_flag(argv, [Grammar, Pipeline, Input]),
    load_grammar_file(Grammar),
    run_pipeline(Pipeline, filename, Input, _).

main_or_halt :-
    catch(main, Error, (print_message(error, Error), halt(1))).

:- initialization(main_or_halt).
