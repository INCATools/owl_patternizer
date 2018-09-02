/**

  tests owl_patterns

*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(owl_patternizer)).

:- rdf_register_prefix('UBERON','http://purl.obolibrary.org/obo/UBERON_').
:- rdf_register_prefix('MONDO','http://purl.obolibrary.org/obo/MONDO_').
:- rdf_register_prefix('RO','http://purl.obolibrary.org/obo/RO_').

:- begin_tests(pattern_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        rdf_load('tests/data/neoplasm.owl').

test(w) :-
        %forall(ground_expression(X),
        %       writeln(X)),
        generate_patterns([min(5),
                           trim(true),
                           base('http://purl.obolibrary.org/obo/myest'),
                           exclude_prefixes(['UBERON'])
                          ]).


:- end_tests(pattern_test).


