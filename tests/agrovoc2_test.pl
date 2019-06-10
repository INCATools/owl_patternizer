/**

  tests owl_patterns

*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(owl_patternizer)).

:- rdf_register_prefix(agrovoc,'http://aims.fao.org/aos/agrontology#').
:- rdf_register_prefix(skos,'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_prefix(skosxl,'http://www.w3.org/2008/05/skos-xl#').

:- begin_tests(pattern_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        rdf_load('tests/data/agrovoc_merged.owl'),
        load_import_closure.



test(write) :-
        debug(patternizer),
        generate_patterns([min(2),
                           dir('target/agrovoc'),
                           trim(true)
                          ]).






:- end_tests(pattern_test).


