/**

  tests owl_patterns

*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(owl_patternizer)).
:- use_module(library(owl_patternizer/definition_inference)).

:- [stacktrace].

:- debug(def).


:- rdf_register_prefix('ENVO','http://purl.obolibrary.org/obo/ENVO_').

:- begin_tests(envo_mkdef_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        rdf_load('tests/data/envo_test.ttl'),
        load_import_closure,
        definition_inference:create_bitmap_index.

test(sub) :-
        PC = 'http://purl.obolibrary.org/obo/ENVO_01001526',
        best_parse_class(PC,P,Matches,Score),
        format('~w ==> ~q Matches: [~w] Score= ~w~n',[PC,Matches,P,Score]),
        matches_to_class_expression(Matches,Expr),
        format('Expr: ~q~n',[Expr]),
        assertion( Expr=and(['http://purl.obolibrary.org/obo/ENVO_00001998',some('http://purl.obolibrary.org/obo/RO_0000086','http://purl.obolibrary.org/obo/PATO_0001985')]) ).


xxtest(do_all) :-
        do_all.

do_all :-
        owl:class(PC),
        best_parse_class(PC,P,Matches,Score),
        format('~w ==> ~w [~w] // ~w~n',[PC,Matches,P,Score]),
        assert_inferred_equiv_axioms(PC,gen),
        % save as we go
        rdf_save_turtle('target/gen-ENVO.ttl',[graph(gen)]),
        fail.
do_all.




:- end_tests(envo_mkdef_test).


