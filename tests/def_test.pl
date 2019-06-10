/**

  tests definition_inference, i.e. inferring logical def based on both lexical strings and logical axioms

*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(rdf_matcher)).
:- use_module(library(owl_patternizer)).
:- use_module(library(owl_patternizer/definition_inference)).

:- rdf_register_prefix('UBERON','http://purl.obolibrary.org/obo/UBERON_').
:- rdf_register_prefix('MONDO','http://purl.obolibrary.org/obo/MONDO_').
:- rdf_register_prefix('RO','http://purl.obolibrary.org/obo/RO_').
:- rdf_register_prefix('BFO','http://purl.obolibrary.org/obo/BFO_').

:- debug(def).
:- debug(index).
:- debug(counter).


:- begin_tests(def_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        rdf_load('tests/data/neoplasm.owl'),
        load_import_closure,
        create_bitmap_index.

do_all :-
        owl:class(PC),
        best_parse_class(PC,P,Matches,Score),
        format('~w ==> ~w [~w] // ~w~n',[PC,Matches,P,Score]),
        assert_inferred_equiv_axioms(PC,gen),
        % save as we go
        rdf_save_turtle('target/gen.ttl',[graph(gen)]),
        fail.
do_all.

xxxxtest(exclude) :-
        assertion( owl_patternizer:exclude('http://purl.obolibrary.org/obo/MONDO_0005138', [ontology_prefix('UBERON')]) ),
        assertion( \+ owl_patternizer:exclude('http://purl.obolibrary.org/obo/MONDO_0005138', [ontology_prefix('MONDO')]) ).

        

test(expr) :-
        PC = 'http://purl.obolibrary.org/obo/MONDO_0005138',
        best_parse_class(PC,P,Matches,Score),
        format('~w ==> ~q Matches: [~w] Score= ~w~n',[PC,Matches,P,Score]),
        matches_to_class_expression(Matches,Expr),
        format('Expr: ~q~n',[Expr]),
        assertion( Expr=and(['http://purl.obolibrary.org/obo/MONDO_0004993',some('http://purl.obolibrary.org/obo/RO_0004026','http://purl.obolibrary.org/obo/UBERON_0002048')]) ).

test(subterm) :-
        PC = 'http://purl.obolibrary.org/obo/MONDO_0006392',
        forall(make_class_subterm_link(PC,Parent,sc,[new_class_prefix('http://x.org/')]),
               format('Parent = ~w~n',[Parent])),
        rdf_save('target/subterm_gen.ttl',[graph(sc),format(turtle)]).




xxxtest(do_all) :-
        do_all.


        
xxxtest(sub) :-
        owl:class(PC),
        rdf(PC,rdfs:label,LabelLit),
        ensure_atom(LabelLit,A),
        %atom_class_token(A,C,Pre,Post),
        best_parse_atom(PC,A,Matches,Score),
        %format('~w ==> ~w ~w ~w~n',[A,Pre,C,Post]),
        format('~w ==> ~w // ~w~n',[A,Matches,Score]),
        fail.

:- end_tests(def_test).


