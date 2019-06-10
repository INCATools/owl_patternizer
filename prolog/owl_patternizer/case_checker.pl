/** <case_checker>

  Infers correct case usage (capitalization, lowercase, initialism) for literals
  
  EXPERIMENTAL

  
  
*/

:- module(case_checker,
          [ix/0,
           usage/2,
           usage_from_triple/4,
           class_conflicting_usage/5,
           conflicting_usage/4,
           conflicting_usages/1]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(tabling)).
:- use_module(library(index_util)).

% TODO: make configurable
:- rdf_register_prefix(def,'http://purl.obolibrary.org/obo/IAO_0000115').
:- rdf_register_prefix(oio,'http://www.geneontology.org/formats/oboInOwl#').

ix :-
        materialize_index(usage(+,+)).
ix(File) :-
        materialize_index_to_file(usage(+,+), File).


%:- table usage/2.
usage(W,Usage) :-
        usage_from_triple(_,_,_, Usage),
        Usage=usage(W,_,_,_,_,_).


%! usage_from_triple(?S, ?P, ?O, ?Usage) is nondet
%
%    given a triple, infer datapoints about case-usage
%
:- rdf_meta usage_from_triple(r,r,r,?).
%:- table usage_from_triple/4.
usage_from_triple(S,P,O, Usage) :-
        rdf(S,P,O),
        ensure_atom(O, Term),
        \+ deprecated(S),
        uri_prefix(S,Prefix1),
        downcase_atom(Prefix1,Prefix),
        prefix_property_expected_weight(Prefix, P, Style, Weight),
        usage_from(Style, Weight, Prefix, Term, P, Usage).

%! usage_from(+Style, +Weight, +Prefix, +Term, +Prop, ?Usage) is nondet
%
%   e.g (capitalized, 8, hp, 'Abnormal foo', )
%
usage_from(Style, Weight, Prefix, Term, Prop, Usage) :-
        re_split_atom("([\\.:;,/-]\\s*|\\s+)", Term, Tokens),
        include([X]>>(\+atom_concat(' ',_,X)),Tokens,Tokens2),
        tokens_to_pairs(Tokens2, start, Pairs),
        member(Word-Last,Pairs),
        token_category(Word,Type),
        categorize_occurrence(Last,Type,Style,Class),
        downcase_atom(Word,WordDn),
        Usage=usage(WordDn,Class,Word,Prop,Prefix,Weight).

%! token_category(+Token:atom, ?Category) is det
%
%   categorize a token lexically, one of: initialism, capitalized, lowercase, unknown
token_category(Token,initialism) :-  re_match("^[A-Z].*[A-Z]",Token),!.
token_category(Token,capitalized) :-  re_match("^[A-Z]",Token),!.
token_category(Token,lowercase) :-  re_match("^[a-z]+$",Token),!.
token_category(_,unknown).

%! categorize_occurrence(+Pre, +TokenCategory, +Expected, ?InferredClass)
%
% given a pair of (previous_token, token_category), plus
% the context, categorize whether this provides evidence for or against
% a class
categorize_occurrence(start,capitalized,sentence,not(initialism)).
categorize_occurrence(stop,capitalized,sentence,not(initialism)).
categorize_occurrence(_,capitalized,all_capitalized,not(initialism)).
categorize_occurrence(_,capitalized,lowercase,capitalized).

categorize_occurrence(_,initialism,sentence,initialism).
categorize_occurrence(_,initialism,lowercase,initialism).
categorize_occurrence(_,initialism,all_capitalized,initialism).

categorize_occurrence(_,lowercase,_,lowercase).

:- rdf_meta obosyn(r).
obosyn(oio:hasRelatedSynonym).
obosyn(oio:hasBroadSynonym).
obosyn(oio:hasNarrowSynonym).
obosyn(oio:hasExactSynonym).

:- rdf_meta prefix_property_expected_weight(?,r,?,?).
% TODO: learn these rather than hardcode
prefix_property_expected_weight(mesh, def:'', sentence, 1) :- !.
prefix_property_expected_weight(_, def:'', sentence, 6) :- !.

prefix_property_expected_weight(mondo, rdfs:label, lowercase, 7) :- !.
prefix_property_expected_weight(mondo, P, lowercase, 4) :- obosyn(P), !.
prefix_property_expected_weight(mp, rdfs:label, lowercase, 8) :- !.
prefix_property_expected_weight(mp, P, lowercase, 8) :- obosyn(P), !.
prefix_property_expected_weight(go, rdfs:label, lowercase, 8) :- !.
prefix_property_expected_weight(go, P, lowercase, 8) :- obosyn(P), !.
prefix_property_expected_weight(uberon, rdfs:label, lowercase, 8) :- !.
prefix_property_expected_weight(uberon, P, lowercase, 8) :- obosyn(P), !.
prefix_property_expected_weight(cl, rdfs:label, lowercase, 7) :- !.
prefix_property_expected_weight(cl, P, lowercase, 7) :- obosyn(P), !.
prefix_property_expected_weight(hp, rdfs:label, sentence, 8) :- !.
prefix_property_expected_weight(hp, P, sentence, 8) :- obosyn(P), !.
prefix_property_expected_weight(ncit, rdfs:label, all_capitalized, 8) :- !.
prefix_property_expected_weight(ncit, P, all_capitalized, 2) :- obosyn(P), !.
prefix_property_expected_weight(omim, _, uppercase, 8) :- !.
prefix_property_expected_weight(doid, rdfs:label, lowercase, 7) :- !.

% true if two usage classifications are in conflict
conflict(not(X),X).
conflict(X,not(X)).
conflict(X,Y) :- X\=Y, atom(X), atom(Y), X\=unknown, Y\=unknown.

%! conflicting_usage(?Word, ?Score, ?Usage1, ?Usage2) is nondet
conflicting_usage(W,S,U1,U2) :-
        U1 = usage(_,C1,_,_,_,S1),
        U2 = usage(_,C2,_,_,_,S2),
        usage(W,U1),
        usage(W,U2),
        conflict(C1,C2),
        S is S1 * S2,
        S > 5.


conflicting_usages(L) :-
        setof(S-c(W,U1,U2),conflicting_usage(W,S,U1,U2),L1),
        reverse(L1,L).

unanimous_usage(W,C,S,U) :-
        U = usage(_,C,_,_,_,S),
        usage(W,U),
        S > 6,
        \+ ((usage(W,U2),
             U2 = usage(_,C2,_,_,_,S2),
             S2 > 4,
             conflict(C,C2))).

class_conflicting_usage(C,W,S,U1,U2) :-
        usage_from_triple(C,_,_,U1),
        usage_from_triple(C,_,_,U2),
        conflicting_usage(W,S,U1,U2).


% ========================================
% UTIL PREDS
% ========================================
tokens_to_pairs([], _, []).
tokens_to_pairs([Token|Tokens], _, Pairs) :-
        atom_concat('.',_,Token),
        !,
        tokens_to_pairs(Tokens, stop, Pairs).
tokens_to_pairs([Token|Tokens], LastToken, [Token-LastToken|Pairs]) :-
        tokens_to_pairs(Tokens, Token, Pairs).

re_split_atom(P,In,Tokens) :-
        re_split(P, In, StrTokens),
        maplist([S,A]>>atom_string(A,S),StrTokens,Tokens).
        
%! uri_prefix(?URI:atom, ?Prefix:atom)
uri_prefix(URI,Prefix) :-
        rdf_global_id(Prefix:_,URI).
