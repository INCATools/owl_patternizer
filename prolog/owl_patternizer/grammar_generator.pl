:- module(grammar_generator,
          []).

:- use_module(library(owl_patternizer)).
:- use_module(library(rdf_matcher)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yaml)).

gen( (term(Var) --> String), Obj) :-
    Vars=Obj.vars,
    member(Var,Vars),
    Root=Obj.classes.Var,
    rdfs_subclass_of(C,Root),
    rdf(C,rdfs:label,Lit)
