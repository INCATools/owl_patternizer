/**

  tests owl_patterns

*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(owl_patternizer)).

:- [stacktrace].


:- rdf_register_prefix('MSO','http://purl.obolibrary.org/obo/MSO_').

:- begin_tests(dl_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        rdf_load('tests/data/mso_test.ttl'),
        load_import_closure.




test(write_pseudotest) :-
        % TODO: make this an actual test with assertions
        debug(patternizer),
        generate_patterns([min(2),
                           max_class_signature(6),
                           generalize_properties(false),
                           dir('target/mso'),
                           base('http://x.org/')
                          ]).

:- end_tests(dl_test).


