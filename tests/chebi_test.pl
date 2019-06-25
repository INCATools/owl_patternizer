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

:- rdf_register_prefix('CHEBI','http://purl.obolibrary.org/obo/CHEBI_').

:- debug(def).
%:- debug(xdef).
:- debug(index).
:- debug(counter).


:- begin_tests(chebi_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        rdf_load('tests/data/citrate.owl'),
        debug(def,'Mutating chebi...',[]),
        mutate_chebi,
        rdf_save_turtle('target/chebi_enhanced.ttl',[]),
        debug(def,'Creating index...',[]),
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

test(conjbase) :-
        _ChargeCls='http://purl.obolibrary.org/obo/CHEBI_-1',
        C='http://purl.obolibrary.org/obo/CHEBI_35804',
        forall(definition_inference:parse_class(C,P,Matches,Score),
               format('*** ~w ==> ~w [~w] // ~w~n',[C,Matches,P,Score])),
        true.
        

save_rdf :-
        rdf_save_turtle('target/chebi_merged.ttl',[]).

        

:- end_tests(chebi_test).


