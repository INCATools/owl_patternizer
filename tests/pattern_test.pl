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
:- rdf_register_prefix('BFO','http://purl.obolibrary.org/obo/BFO_').

:- begin_tests(pattern_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        rdf_load('tests/data/neoplasm.owl'),
        load_import_closure.

test(exclude) :-
        V1='$VAR'(1),
        V2='$VAR'(2),
        X1 = and([V1, some(V2, v)]),
        assertion( owl_patternizer:exclude(X1, [generalize_properties(false)]) ),
        assertion( \+ owl_patternizer:exclude(X1, []) ),

        X1b = and([V1, some(p, V2)]),
        assertion( \+ owl_patternizer:exclude(X1b, [generalize_properties(false)]) ),

        X2 = and([a,b,c,d,e,f,g]),
        assertion( owl_patternizer:exclude(X2, [max_class_signature(6), max_and_cardinality(6)]) ),
        assertion( \+ owl_patternizer:exclude(X2, [max_class_signature(8), max_and_cardinality(8)]) ),
        
        X3 = and([a,some(V1,V2)]),
        assertion( owl_patternizer:exclude(X3, [trim(true)]) ),
        assertion( \+owl_patternizer:exclude(X3, [trim(false)]) ),
        nl.
        
test(infer_range) :-
        V1='$VAR'(1),
        V2='$VAR'(2),
        X = and([V1, some(LProp, V2)]),
        
        forall((rdf(LProp,rdf:type,owl:'ObjectProperty'),
                infer_pattern_var_range(X,V,Range)),
               format('RANGE: ~w ~w ~w~n',[X,V,Range])),

        % 'subdivision of skeleton' should be inferred range for V1 when P=skeleton-of
        assertion( (LProp='http://purl.obolibrary.org/obo/RO_0002576',
                    infer_pattern_var_range(X,1,['http://purl.obolibrary.org/obo/UBERON_0010912']))),
        nl.

test(induce) :-
        rdf_global_id(rdfs:label, RdfsLabel),
        LProp='http://purl.obolibrary.org/obo/RO_0004026',
        X = and(['$VAR'(1), some(LProp, '$VAR'(2))]),
        nl,
        forall(induce_annotation_pattern_with_freq(X, _, AProp, Fmt, Count),
               format('  ~w => ~w [~w]~n',[AProp,Fmt,Count])),

        % free
        assertion(induce_annotation_pattern(X, _, AProp, fmt('%s %s',[2,1]))),
        assertion(induce_annotation_pattern(X, _, AProp, fmt('%s of %s',[1,2]))),

        % ground
        assertion(induce_annotation_pattern(X, _, RdfsLabel, fmt('%s %s',[2,1]))),
        assertion(induce_annotation_pattern(X, _, RdfsLabel, fmt('%s of %s',[1,2]))),
        
        forall(induce_best_annotation_pattern(X, _, AProp, Fmt, Count),
               format('  BEST ~w => ~w [~w]~n',[AProp,Fmt,Count])),
        forall(induce_best_annotation_pattern(X, _, RdfsLabel, Fmt, Count),
               format('  BEST ( label) ~w => ~w [~w]~n',[AProp,Fmt,Count])),
        assertion(induce_best_annotation_pattern(X, _, RdfsLabel, fmt('%s %s',[2,1]), _)),
        nl.



test(write_pseudotest) :-
        % TODO: make this an actual test with assertions
        debug(patternizer),
        generate_patterns([min(5),
                           dir(target),
                           trim(true),
                           base('http://purl.obolibrary.org/obo/myest'),
                           exclude_prefixes(['UBERON'])
                          ]).

test(unify) :-
        V1='$VAR'(1),
        V2='$VAR'(2),
        V3='$VAR'(3),
        
        % basic
        assertion( unify(foo, V1, [1=foo]) ),
        assertion( unify(foo, foo, []) ),
        assertion( \+ unify(foo, '$VAR'(1), [1=bar]) ),
        assertion( \+ unify(foo, bar, _) ),

        % svf
        assertion( unify(some(p,v), some(p,V1),  [1=v]) ),
        assertion( unify(some(p,v), some(V1,V2),  [1=p, 2=v]) ),

        % intersection
        % also test for order-independence
        assertion( unify(and([a,b]), and([a,V1]),  [1=b]) ),
        assertion( unify(and([b,a]), and([a,V1]),  [1=b]) ),
        assertion( unify(and([a,b]), and([V1,a]),  [1=b]) ),
        assertion(( unify(and([a,b]), and([V1,V2]), L1),
                    (   L1=[1=a, 2=b] ; L1=[1=b, 2=a] ))),

        % nesting
        assertion( unify(and([genus,some(p,v)]), and([genus,some(p,v)]),  []) ),
        assertion( unify(and([genus,some(p,v)]), and([genus,some(V1,v)]),  [1=p]) ),
        assertion( unify(and([genus,some(p,v)]), and([genus,some(V1,V2)]),  [1=p, 2=v]) ),
        assertion( unify(and([genus,some(p,v)]), and([V1,some(V2,V3)]),  [1=genus, 2=p, 3=v]) ),
        assertion( \+ unify(and([genus,some(p,v)]), and([genus,some(p,v2)]),  _) ),

        % deeper nesting: G and P some (P2 some V2)
        assertion( unify(and([genus,some(p,some(p2,v2))]), and([V1,some(p,some(V2,V3))]),  [1=genus, 2=p2, 3=v2]) ),
        !.

test(unify_deep) :-        
        V1='$VAR'(1),
        V2='$VAR'(2),
        V3='$VAR'(3),
        AND =  and([a,
                    and([b,
                         and([d,e,f]),
                         and([g,
                              and([h,i])])])]),
        unify( AND,
               and([
                    and([and([f,e,V2]),
                         b,
                         and([g,
                              and([i,V3])])]),
                    V1]),
               L1),
        assertion(permutation(L1, [1=a, 2=d, 3=h])),
        unify( AND,
               and([
                    and([and([V1,V2,V3]),
                         b,
                         and([g,
                              and([i,h])])]),
                    a]),
               L2),
        assertion(permutation(L2, [1=d, 2=e, 3=f])),
        !.





:- end_tests(pattern_test).


