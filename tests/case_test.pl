/**

  tests owl_patterns

*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(sparqlprog)).
:- use_module(library(owl_patternizer/case_checker)).

:- rdf_register_prefix('UBERON','http://purl.obolibrary.org/obo/UBERON_').
:- rdf_register_prefix('MONDO','http://purl.obolibrary.org/obo/MONDO_').
:- rdf_register_prefix('NCIT','http://purl.obolibrary.org/obo/NCIT_').
:- rdf_register_prefix('MP','http://purl.obolibrary.org/obo/MP_').
:- rdf_register_prefix('HP','http://purl.obolibrary.org/obo/HP_').
:- rdf_register_prefix('RO','http://purl.obolibrary.org/obo/RO_').
:- rdf_register_prefix('BFO','http://purl.obolibrary.org/obo/BFO_').

:- debug(index).

:- begin_tests(case_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        rdf_load('tests/data/thyroid_lextest.ttl').

test(case) :-
        forall(usage(W,X),
              writeln(W-X)).
test(conflict) :-
        conflicting_usages(L),
        forall(member(S-c(W,U1,U2),L),
               format('~w\t~w\t~w\t~w~n',[S,W,U1,U2])).





        

:- end_tests(case_test).


