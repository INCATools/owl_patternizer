/** <module> definition_analyzer

  Parses textual definitions.

  Attempt to match parsed textual definition to logical definition, to test for concordance
  
*/
:- module(definition_analyzer,
          [compile_terms/0,
           def_parse/2,
           cls_tdef/2,
           eval_class_def/3]).

:- use_module(library(owl_patternizer)).
:- use_module(library(rdf_matcher)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/dataframe)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(tabling)).
:- use_module(library(index_util)).
:- use_module(library(index_util)).

:- rdf_register_prefix(oio,'http://www.geneontology.org/formats/oboInOwl#').
:- rdf_register_ns(def,'http://purl.obolibrary.org/obo/IAO_0000115').

dataframe:dataframe(definition_analysis,
                    [
                     [class=C,
                      score=S,
                      info=Info]-eval_class_def(C,S,Info),
                     [text_def=D]-cls_tdef(C,D)
                    ],
                    [entity(class)]).


eval_class_def(C,BestScore,BestInfo) :-
        ldef(C,_),
        (   setof(Score-Info,eval_class_def_candidate(C,Score,Info),Pairs)
        ->  debug(def,'~w == Pairs: ~w',[C,Pairs]),
            reverse(Pairs,[BestScore-BestInfo|_])
        ;   BestScore=0, BestInfo=no_parse).

cls_tdef(C,D) :-
        rdf(C,def:'',D1),
        ensure_atom(D1,D).

eval_class_def_candidate(C,Score,Info) :-
        ldef(C,LogDef),
        cls_tdef(C,TD),
        def_parse(TD,X),
        debug(def,'Eval: ~w ~w',[C,TD]),
        compare_text_logic(X,LogDef,Score,Info),
        debug(def,'Score: ~w ~w',[C,Score]).
        

ldef(C,LogDef) :-
        rdf(C,owl:equivalentClass,LogDef),
        rdf_is_bnode(LogDef).

        
compare_text_logic(X,LogDef,Score,info(X,LogDef,SI)) :-
        logdef_classes(LogDef,Cs1),
        parse_classes(X,Cs2),
        ord_intersection(Cs1,Cs2,SI),
        ord_union(Cs1,Cs2,SU),
        length(SI,LenSI),
        length(SU,LenSU),
        debug(simj,'  Common: ~w / ~w',[SI,SU]),
        Score is LenSI/LenSU.

logdef_classes(LogDef,Cs) :-
        setof(C,(bnode_signature(LogDef,C),rdf(C,rdf:type,owl:'Class')),Cs).
parse_classes(X,Cs) :-
        setof(C,parse_sig(X,C),Cs).

parse_sig(def(G,_Diff),C) :-
        parse_sig(G,C).
parse_sig(def(_G,Diff),C) :-
        parse_sig(Diff,C).
parse_sig(term(C,_),C).
parse_sig(combo(X,_,_),C) :-
        parse_sig(X,C).
parse_sig(combo(_,_,X),C) :-
        parse_sig(X,C).




        


def_parse(Def,X) :-
        downcase_atom(Def,Def1),
        atom_codes(Def1,Codes),
        phrase(definition(X), Codes).

compile_terms :-
        pmap(PN,P),
        rdf(C,P,Label),
        ensure_atom(Label,Label1),
        downcase_atom(Label1,Label2),
        compile_term(C,Label2,PN),
        fail.
compile_terms.

compile_term(C,N,PN) :-
        atom_codes(N,Codes1),
        append(Codes1,Rest,Codes),
        assert( controlled_term(term(C,PN),Codes,Rest) ).


:- rdf_meta pmap(-,r).
pmap(label, rdfs:label).
pmap(label, skos:prefLabel).
pmap(related, oio:hasRelatedSynonym).
pmap(exact, oio:hasExactSynonym).
pmap(broad, oio:hasBroadSynonym).
pmap(narrow, oio:hasNarrowSynonym).

% ----------------------------------------
% grammar
% ----------------------------------------

definition(D) --> genus_differentia_definition(D), opt_stop.
%TODOdefinition(D) --> union_definition(D).

genus_differentia_definition( def(G,Diff) ) --> opt_copula_ws, genus(G), opt_ws, connector, opt_ws, differentia(Diff).

opt_stop --> `.`, ignore_rest.
opt_stop --> [].

opt_sep_copula --> ` `, copula, ` `.
opt_sep_copula --> ` `.

opt_copula_ws --> copula, ` `.
opt_copula_ws --> [].
copula --> `a` ; `an` ; `the`.

ignore_rest --> [_],!.
ignore_rest --> [].


genus(G) --> compound_term(G).
relation(G) --> atomic_term(G).

compound_term(combo(X,Op,Y)) --> atomic_term(X), logop(Op), !, compound_term(Y).
compound_term(X) --> atomic_term(X).

atomic_term(X) --> controlled_term(X).
atomic_term( text(A) ) --> words(TL), {atom_codes(A,TL)}.


words( [X|L] ) --> word(X), words(L).
words( [X] ) --> word(X).

word(X) --> [X], {X\=0'.}.



opt_ws --> ws, !.
opt_ws --> [].

ws --> ws_char, !, ws.
ws --> [].
ws_char --> ` `.

connector --> `that` ; `which`.

%opt_prep 
prep --> `in`.
prep --> `by`.

logop(and) --> `, and `.
logop(or) --> `, or `.
logop(and) --> ` and `.
logop(unknown) --> `, `.
logop(or) --> ` or `.
logop(or) --> `; `.

differentia(X) --> multiple_structured_differentia(X).
differentia( unstructured(X) ) --> \+ structured_differentia(_), !, words(X).

multiple_structured_differentia(combo(X,Op,Y)) --> structured_differentia(X), logop(Op), multiple_structured_differentia(Y).
multiple_structured_differentia(X) --> structured_differentia(X).

structured_differentia( some(R,Y) ) --> relation(R), opt_sep_copula, compound_term(Y).
