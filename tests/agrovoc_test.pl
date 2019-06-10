/**

  tests owl_patterns

*/

:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(rdf_matcher)).
:- use_module(library(owl_patternizer)).
:- use_module(library(owl_patternizer/definition_inference)).

:- rdf_register_prefix(agrovoc,'http://aims.fao.org/aos/agrontology#').
:- rdf_register_prefix(skos,'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_prefix(skosxl,'http://www.w3.org/2008/05/skos-xl#').

:- debug(def).
:- debug(index).


:- begin_tests(agrovoc_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        rdf_load('tests/data/agrovoc_owl.ttl'),
        debug(def,'Creating index...',[]),
        index_pairs,
        debug(def,'Creating BM index...',[]),
        create_bitmap_index.
        

do_all :-
        owl:class(PC),
        rdfs_label(PC,Name),
        best_parse_class(PC,P,Matches,Score),
        format('~w "~w" ==> ~w [~w] // ~w~n',[PC,Name,Matches,P,Score]),
        assert_inferred_equiv_axioms(PC,gen),
        % save as we go - TODO - only do this on change
        random(Rand),
        (   Rand < 0.1
        ->  save_rdf
        ;   true),
        fail.

do_all :- save_rdf.

test(sub) :-
        do_all.

save_rdf :-
        rdf_save_turtle('target/agrovoc_ec2.ttl',[graph(gen)]).

        

:- end_tests(agrovoc_test).


