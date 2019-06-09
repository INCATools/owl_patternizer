/** <module> skos2owl

  converts skos into owl
  
  
*/
:- module(skos2owl,
          [inject_owl_axioms/1]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(owl_patternizer/definition_inference)).

:- rdf_register_prefix(skos,'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_prefix(skosxl,'http://www.w3.org/2008/05/skos-xl#').

%! inject_owl_axioms(+Graph) is det.
inject_owl_axioms(G2) :-
        T=rdf(S,P,O,_G),
        findall(T,(T,transform(S,P,O,G2)),TrTriples),
        length(TrTriples,Len),
        debug(skos,'Trasnformed: ~w',[Len]).


:- rdf_meta transform(r,r,o,r).
transform(S,P,O,G) :-
        rdfs_individual_of(P,rdf:'Property'),
        \+ non_object_property(P),
        rdf_is_iri(S),
        rdf_is_iri(O),
        rdf(S,rdf:type,skos:'Concept'),
        rdf(O,rdf:type,skos:'Concept'),
        !,
        debug(skos,'Saving ~w Sub ~w some ~w',[S,P,O]),
        save_axiom(subClassOf(S,some(P,O)),G).

transform(S,skos:broader,O,G) :-
        !,
        save_axiom(subClassOf(S,O),G).
transform(S,skos:related,O,G) :-
        !,
        save_axiom(subClassOf(S,some(skos:related,O)),G).

transform(S,skos:prefLabel,O,G) :-
        \+ rdf_is_iri(O),
        rdf_assert(S,rdfs:label,O,G).
transform(S,skosxl:prefLabel,O,G) :-
        rdf_is_iri(O),
        rdf(O,skosxl:literalForm,Lit),
        % TODO: make language configurable
        Lit = _@en,
        !,
        debug(skos,'Ann ~w ~w',[S,Lit]),
        rdf_assert(S,rdfs:label,Lit,G).
transform(S,rdf:type,skos:'Concept',G) :-
        rdf_is_iri(S),
        !,
        rdf_assert(S,rdf:type,owl:'Class',G).
transform(S,P,O,G) :-
        rdf_is_iri(O),
        rdf(O,skosxl:literalForm,Lit),
        % TODO: make language configurable
        Lit = _@en,
        !,
        debug(skos,'Ann ~w ~w ~w',[S,P,Lit]),
        rdf_assert(S,P,Lit,G).

transform(S,P,O,G) :-
        passthru(P),
        rdf_assert(S,P,O,G).

non_object_property(P) :-
        rdf_global_id(Prefix:_,P),
        non_op_prefix(Prefix).

non_op_prefix(rdf).
non_op_prefix(rdfs).
non_op_prefix(owl).
non_op_prefix(skos).
non_op_prefix(skosxl).

:- rdf_meta passthru(r).

passthru(skos:exactMatch).
passthru(skos:closeMatch).
passthru(skos:narrowMatch).
passthru(skos:broadMatch).

