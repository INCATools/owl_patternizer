/** <module> definition_analyzer

  Parses textual definitions.

  Attempt to match parsed textual definition to logical definition, to test for concordance
  
*/
:- module(adhoc,
          [ix_rg/0,
           ix_rg1/0,
           relation_genus/2,
           relation_mrca_genus/2,
           relation_mrca_genus/3,
           relation_most_general_class/2
           ]).

:- use_module(library(owl_patternizer)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/dataframe)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(index_util)).

%:- table relation_genus/2.

ix_rg1 :-
        materialize_index(class_differentia(+,+,-)).

ix_rg :-
        materialize_index(class_differentia(+,+,-)),
        materialize_index(relation_genus(+,+)).

relation_genus(R,G) :-
        class_genus(C,G),
        class_differentia(C,R,_).

%! relation_mrca_genus(?Rel, ?GenusClass) is nondet
%
%  find the most general genus term that uses Rel as a relation
%  in a simple genus-differentia pattern
%
%
relation_mrca_genus(R,G) :-
        relation_genus(R,G),
        \+ ((relation_genus(R,G2),
             rdfs_subclass_of(G,G2),
             G2\=G
            )).

relation_mrca_genus(R,G,C) :-
        relation_mrca_genus(R,G),
        class_genus(C,G),
        class_differentia(C,R,_).

relation_most_general_class(R,C) :-
        class_differentia(C,R,_),
        \+ ((class_differentia(C2,R,_),
             rdfs_subclass_of(C,C2),
             C2\=C
            )).

             
