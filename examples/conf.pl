/**

  ----------------------------------------
  GENERATE EXAMPLES
  ----------------------------------------
  
  Auto-generate YAML DOSDP patterns for a selected range of ontologies

  See the Makefile for how to run this.

  Wanted: more non-OBO ontologies to use as examples
  - should be somewhat obo-esque i.e. compositional, with lots of classes with logical defs, following EL-ish structure
  
*/

:- use_module(library(owl_patternizer)).

:- rdf_register_prefix('UBERON','http://purl.obolibrary.org/obo/UBERON_').
:- rdf_register_prefix('RO','http://purl.obolibrary.org/obo/RO_').
:- rdf_register_prefix('BFO','http://purl.obolibrary.org/obo/BFO_').
:- rdf_register_prefix('CL','http://purl.obolibrary.org/obo/CL_').
:- rdf_register_prefix('PATO','http://purl.obolibrary.org/obo/PATO_').
:- rdf_register_prefix('GO','http://purl.obolibrary.org/obo/GO_').
:- rdf_register_prefix('EFO','http://www.ebi.ac.uk/efo/EFO_').
:- rdf_register_prefix('CHEBI','http://www.ebi.ac.uk/efo/CHEBI_').
:- rdf_register_prefix('pizza','http://www.co-ode.org/ontologies/pizza/pizza.owl#').
:- rdf_register_prefix('NCIT','http://purl.obolibrary.org/obo/NCIT_').
:- rdf_register_prefix('oio','http://www.geneontology.org/formats/oboInOwl#').

% ----------------------------------------
% settings for each ontology
% ----------------------------------------


ontology_config(pizza,    [min(2), base('http://purl.obolibrary.org/obo/pizza/')]).
ontology_config(pato,     [min(5), generalize_properties(false)]).

ontology_config(wine,     [min(2)]).

ontology_config(so,       [min(8)]).
ontology_config(chebi,    [min(3), generalize_properties(false)]).
ontology_config(uberon,   [min(50), max_and_cardinality(3)]).
ontology_config(cl,       [min(20)]).
ontology_config(fbbt,     [min(50)]).
ontology_config(mondo,    []).
ontology_config(obi,      []).
ontology_config(sctid,    [max_class_signature(4), generalize_properties(false)]).
ontology_config(to,       [min(50)]).
ontology_config(envo,     [min(20)]).
ontology_config(efo,      [min(50)]).
ontology_config(doid,     [min(25)]).
ontology_config(mp,       [min(50), max_class_signature(4), generalize_properties(false)]).
ontology_config(hp,       [min(50), max_class_signature(4), generalize_properties(false)]).
ontology_config(ncit,     [min(50), generalize_properties(false), max_and_cardinality(3)]).
ontology_config(foodon,   []).
ontology_config(go,       [min(25)]).

% ----------------------------------------
% top-level
% ----------------------------------------
doall :-
        forall(ontology_config(Ont,_),
               do_for(Ont)).

do_for(Ont) :-
        rdf_retractall(_,_,_,_),
        rdf_load_library(Ont),
        % assume OBO unless overridden in conf
        atom_concat('http://purl.obolibrary.org/obo/',Ont,DefaultBase),
        ontology_config(Ont,Options),
        % TODO: merge options
        option(min(Min),Options,40),
        option(base(Base),Options,DefaultBase),
        option(generalize_properties(GP),Options,true),
        option(max_and_cardinality(MAC),Options,4),
        option(max_class_signature(MCS),Options,5),
        generate_patterns([min(Min),
                           dir(Ont),
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


        