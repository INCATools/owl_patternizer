/**

  ----------------------------------------
  GENERATE EXAMPLES
  ----------------------------------------
  
  Auto-generate YAML DOSDP patterns for a selected range of ontologies

  See the Makefile for how to run this.

  For some ontologies (specified with infer_axioms(true) we first infer OWL definitions)

  Wanted: more non-OBO ontologies to use as examples
  - should be somewhat obo-esque i.e. compositional, with lots of classes with logical defs, following EL-ish structure
  
*/

:- use_module(library(owl_patternizer)).
:- use_module(library(owl_patternizer/definition_inference)).
:- use_module(library(owl_patternizer/skos2owl)).

:- use_module(library(rdf_matcher)).
:- use_module(library(sparqlprog/emulate_builtins)).

:- use_module(library(semweb/rdf_cache)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_ntriples)).

:- rdf_register_prefix('UBERON','http://purl.obolibrary.org/obo/UBERON_').
:- rdf_register_prefix('RO','http://purl.obolibrary.org/obo/RO_').
:- rdf_register_prefix('BFO','http://purl.obolibrary.org/obo/BFO_').
:- rdf_register_prefix('CL','http://purl.obolibrary.org/obo/CL_').
:- rdf_register_prefix('PATO','http://purl.obolibrary.org/obo/PATO_').
:- rdf_register_prefix('GO','http://purl.obolibrary.org/obo/GO_').
:- rdf_register_prefix('SO','http://purl.obolibrary.org/obo/SO_').
:- rdf_register_prefix('MSO','http://purl.obolibrary.org/obo/MSO_').
:- rdf_register_prefix('ENVO','http://purl.obolibrary.org/obo/ENVO_').
:- rdf_register_prefix('FOODON','http://purl.obolibrary.org/obo/FOODON_').
:- rdf_register_prefix('EFO','http://www.ebi.ac.uk/efo/EFO_').
:- rdf_register_prefix('CHEBI','http://www.ebi.ac.uk/efo/CHEBI_').
:- rdf_register_prefix('NBO','http://www.ebi.ac.uk/efo/NBO_').
:- rdf_register_prefix('pizza','http://www.co-ode.org/ontologies/pizza/pizza.owl#').
:- rdf_register_prefix('NCIT','http://purl.obolibrary.org/obo/NCIT_').
:- rdf_register_prefix('NIFEXT','http://uri.neuinfo.org/nif/nifstd/nifext_').
:- rdf_register_prefix('oio','http://www.geneontology.org/formats/oboInOwl#').

:- debug(index).
:- debug(autolabel).
:- debug(def).
:- debug(patternizer).

:- [tests/stacktrace].

% ----------------------------------------
% settings for each ontology
% ----------------------------------------


ontology_config(pizza,    [min(2), base('http://purl.obolibrary.org/obo/pizza/')]).
ontology_config(pato,     [infer_axioms(true), min(5), generalize_properties(false), ontology_prefix(pato)]).

ontology_config(wine,     [min(2)]).

ontology_config(so,       [min(8)]).
ontology_config(mso,      [min(5), max_class_signature(6), generalize_properties(false)]).
%ontology_config(chebi,    [min(3), remove_inexact_synonyms(true), infer_axioms(true), generalize_properties(false)]).
ontology_config(chebi,    [min(4), remove_chemical_synonyms(true), mutate_chebi(true), infer_axioms(true), generalize_properties(false)]).
ontology_config(uberon,   [min(50), max_and_cardinality(3)]).
ontology_config(cl,       [min(20), ontology_prefix(cl)]).
ontology_config(nif_cell, [min(10), imports([uberon]), infer_axioms(true), base('http://ontology.neuinfo.org/NIF/ttl/NIF-Cell.ttl'), ontology_prefix(nifext)]).
ontology_config(nif,      [min(20), base('http://ontology.neuinfo.org/NIF/ttl/nif.ttl'),  ontology_prefix(nifext)]).
ontology_config(clo,      [min(20), ontology_prefix(clo)]).
ontology_config(eco,      [min(10), ontology_prefix(eco)]).
ontology_config(fbbt,     [min(50), generalize_properties(false)]).
ontology_config(fbcv,     [infer_axioms(true), min(5), generalize_properties(false)]).
ontology_config(mondo,    []).
ontology_config(geo,      []).
ontology_config(fao,      [infer_axioms(true), min(5), generalize_properties(false)]).
ontology_config(ma,       [infer_axioms(true), min(12), generalize_properties(false)]).
ontology_config(hao,      [infer_axioms(true), min(12), generalize_properties(false)]).
ontology_config(emapa,    [infer_axioms(true), min(12), generalize_properties(false)]).
ontology_config(planaa,   [infer_axioms(true), min(12), generalize_properties(false)]).
ontology_config(zfa,      [infer_axioms(true), min(12), generalize_properties(false)]).
ontology_config(ehdaa2,   [infer_axioms(true), min(12), generalize_properties(false)]).
ontology_config(xao,      [infer_axioms(true), min(12), generalize_properties(false)]).
ontology_config(wbbt,     [infer_axioms(true), min(12), generalize_properties(false)]).
ontology_config(fao,      [infer_axioms(true), min(12), generalize_properties(false)]).
ontology_config(sweet,    [autolabels(true), infer_axioms(true), min(2), generalize_properties(false)]).
ontology_config(svl,      [autolabels(true), infer_axioms(true), min(2), generalize_properties(false)]).
ontology_config(obi,      [min(8), generalize_properties(false)]).
ontology_config(ero,      [min(8), generalize_properties(false)]).
ontology_config(vo,       [min(4), generalize_properties(false)]).
ontology_config(sctid,    [max_class_signature(4), generalize_properties(false)]).
ontology_config(to,       [min(20)]).
ontology_config(envo,     [infer_axioms(true), min(4), generalize_properties(false), ontology_prefix('ENVO'), exclude_prefixes(['FOODON'])]).
ontology_config(agro,     [min(10), infer_axioms(true), generalize_properties(false)]).
ontology_config(efo,      [min(10)]).
ontology_config(doid,     [min(25)]).
ontology_config(idomal,   [min(5), infer_axioms(true)]).
ontology_config(mp,       [min(50), max_class_signature(4), generalize_properties(false)]).
ontology_config(hp,       [min(50), max_class_signature(4), generalize_properties(false)]).
ontology_config(xpo,      [min(50), max_class_signature(4), generalize_properties(false)]).
ontology_config(zp,       [min(200), max_class_signature(4), generalize_properties(false)]).
ontology_config(planp,    [min(50), max_class_signature(4), generalize_properties(false)]).
ontology_config(oae,      [min(10), generalize_properties(false)]).
ontology_config(fypo,     [min(20), generalize_properties(false)]).
ontology_config(omp,      [min(20), generalize_properties(false)]).
ontology_config(apo,      [min(4), infer_links(true), infer_axioms(true), imports([go]), max_class_signature(4), generalize_properties(false)]).
ontology_config(aro,      [min(4), infer_links(true), infer_axioms(true), imports([chebi]), max_class_signature(4), generalize_properties(false)]).
ontology_config(mco,      [min(4), infer_links(true), infer_axioms(true), imports([chebi]), max_class_signature(4), generalize_properties(false)]).
ontology_config(bto,      [min(8), infer_axioms(true), max_class_signature(4), generalize_properties(false)]).
ontology_config(mmo,      [min(8), infer_links(true), infer_axioms(true), max_class_signature(4), generalize_properties(false)]).
ontology_config(mop,      [min(8), infer_links(true), infer_axioms(true), max_class_signature(4), generalize_properties(false)]).
ontology_config(micro,    [min(20), generalize_properties(false)]).
ontology_config(ncit,     [min(50), infer_axioms(true), generalize_properties(false), max_and_cardinality(3)]).
ontology_config(foodon,   [min(10), infer_axioms(true)]).
ontology_config(go,       [min(25)]).
ontology_config(peco,     [min(8), generalize_properties(false)]).
ontology_config(ohmi,     [min(25), generalize_properties(false)]).
ontology_config(vo,       [min(8), generalize_properties(false)]).
ontology_config(nbo,      [min(8), generalize_properties(false), ontology_prefix('NBO')]).
ontology_config(po,       [infer_axioms(true), min(8), generalize_properties(false)]).
ontology_config(tgma,     [infer_axioms(true), min(8), generalize_properties(false)]).
ontology_config(tads,     [infer_axioms(true), min(8), generalize_properties(false)]).
ontology_config(ceph,     [min(8), generalize_properties(false)]).
ontology_config(flopo,    [min(50), generalize_properties(false)]).
ontology_config(agrovoc,  [infer_axioms(true), min(6), generalize_properties(false)]).
ontology_config(mesh,     [infer_axioms(true), min(6), generalize_properties(false)]).
ontology_config(gemet,    [is_skos(true), infer_links(true),new_class_prefix('http://x.org/'), infer_axioms(true), min(5), generalize_properties(false)]).

autolabels :-
        forall(owl:class(C),
               autolabel(C)).

autolabel(C) :-
        concat_atom([_,Frag],'#',C),
        atom_chars(Frag,Chars),
        uncamelify(Chars,Chars2),
        trim_ws(Chars2, Chars3),
        atom_chars(Label, Chars3),
        rdf_assert(C, rdfs:label, Label@en),
        debug(autolabel,'~w label ~w',[C,Label]),
        !.
autolabel(C) :-
        format(user_error,'Could not autolabel: ~w~n',[C]).


uncamelify([], []).
uncamelify([H|T], [' ',H2|T2]) :-
        H @>= 'A',
        H @=< 'Z',
        !,
        downcase_atom(H,H2),
        uncamelify(T,T2).
uncamelify([H|T], [H|T2]) :-
        uncamelify(T,T2).

trim_ws([' '|X],X) :- !.
trim_ws(X,X).


% ----------------------------------------
% top-level
% ----------------------------------------
doall :-
        forall(ontology_config(Ont,_),
               do_for(Ont)).



do_for(Ont) :-
        rdf_attach_library('examples/void.ttl'),
        rdf_retractall(_,_,_,_),
        rdf_load_library(Ont),
        % assume OBO unless overridden in conf
        atom_concat('http://purl.obolibrary.org/obo/',Ont,DefaultBase),
        ontology_config(Ont,Options),

        (   option(autolabels(true),Options,false)
        ->  autolabels
        ;   true),
        
        (   option(remove_inexact_synonyms(true),Options,false)
        ->  remove_inexact_synonyms
        ;   true),

        (   option(remove_chemical_synonyms(true),Options,false)
        ->  remove_chemical_synonyms
        ;   true),

        (   option(mutate_chebi(true),Options,false)
        ->  mutate_chebi
        ;   true),

        
        concat_atom([examples,Ont],'/',DefaultDir),
        option(dir(Dir),Options,DefaultDir),
        make_directory_path(Dir),
        
        concat_atom([Dir,'_src.ttl'], '/', SrcFile),
        rdf_turtle:rdf_save_turtle(SrcFile,[]),
        
        option(is_skos(IsSkos),Options,false),
        (   IsSkos
        ->  inject_owl_axioms(skos2owl),
            concat_atom([Dir,'_owl.ttl'], '/', OwlFile),
            rdf_turtle:rdf_save_turtle(OwlFile,[graph(skos2owl)])
        ;   true),
        
        option(infer_links(IsInferLinks),Options,false),
        (   IsInferLinks
        ->  infer_links(Options),
            concat_atom([Dir,'_links.ttl'], '/', LinksFile),
            rdf_turtle:rdf_save_turtle(LinksFile,[graph(sc)])
        ;   true),
        
        option(infer_axioms(IsInfer),Options,false),
        (   IsInfer
        ->  create_bitmap_index,
            assert_inferred_equiv_axioms(_,gen,[ new_only(true)|Options ]),
            concat_atom([Dir,'_induced_axioms.ttl'], '/', AxFile),
            rdf_turtle:rdf_save_turtle(AxFile,[graph(gen)]),
            concat_atom([Dir,'_induced_axioms_merged.ttl'], '/', MergedAxFile),
            rdf_turtle:rdf_save_turtle(MergedAxFile,[])
        ;   true),

        concat_atom([Dir,'_input.ttl'], '/', OwlFile),
        rdf_turtle:rdf_save_turtle(OwlFile,[]),
            
        % TODO: merge options
        option(min(Min),Options,40),
        option(base(Base),Options,DefaultBase),
        option(generalize_properties(GP),Options,true),
        option(max_and_cardinality(MAC),Options,4),
        debug(patternizer,'GENERATING',[]),
        option(max_class_signature(MCS),Options,5),
        generate_patterns([min(Min),
                           dir(Dir),
                           trim(true),
                           base(Base),
                           generalize_properties(GP),
                           max_and_cardinality(MAC),
                           max_class_signature(MCS),
                           annotations([
                                        ann(exact_synonym, oio:hasExactSynonym, 0.05),
                                        ann(related_synonym, oio:hasRelatedSynonym, 0.05)
                                        ])
                          ]).



        
