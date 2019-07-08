/**

  tests owl_patterns

*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/labelutils)).
:- use_module(library(index_util)).
:- use_module(library(rdf_matcher)).
:- use_module(library(owl_patternizer)).
:- use_module(library(owl_patternizer/definition_inference)).

:- [stacktrace].

:- debug(def).
:- debug(index).


:- rdf_register_prefix('ENVO','http://purl.obolibrary.org/obo/ENVO_').

:- begin_tests(envo_lexdiff_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        rdf_load('tests/data/envo_test.ttl'),
        load_import_closure,
        remove_inexact_synonyms,
        index_entity_tokens.

        

test(d) :-
        G = tokens_delta_class_pair_rel([biome],[ecosystem],_C1,_C2,_,_,_Rels),
        forall((G,term_labelify(G,X)),
               writeln(X)).


xxxtest(d) :-
        G = label_delta_class_pair(' biome',_,_C1,_C2,_,_),
        forall((G,term_labelify(G,X)),
               writeln(X)).






:- end_tests(envo_lexdiff_test).


