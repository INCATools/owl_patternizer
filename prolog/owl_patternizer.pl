:- module(owl_patternizer,
          [ground_expression/1,
           generate_patterns/1]).

:- use_module(library(sparqlprog/ontologies/owl), []).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(tabling)).

% ========================================
% MAP RDF
% ========================================

:- table ground_expression/2.
ground_expression(X) :-
        ground_expression(_, X).
ground_expression(C, X) :-
        rdf(C,owl:equivalentClass,Node),
        \+ rdf_is_bnode(C),
        rdf_is_bnode(Node),
        node_expression(Node,X).

node_expression(Node,Node) :-
        \+ rdf_is_bnode(Node).

node_expression(Node,and(Xs)) :-
        rdf(Node,owl:intersectionOf,L),
        !,
        setof(X,rdflist_member_expression(L,X),Xs).

node_expression(Node,some(P,V)) :-
        rdf(Node,owl:onProperty,PNode),
        rdf(Node,owl:someValuesFrom,VNode),
        !,
        node_expression(PNode,P),
        node_expression(VNode,V).

rdflist_member_expression(L,X) :-
        rdflist_member(L,M),
        node_expression(M,X).

% ========================================
% GENERALIZING EXPRESSIONS
% ========================================

generalize_expression(X,X2) :- generalize_expression(X,X2, classExpression).


generalize_expression(V, _, _) :- var(V),!,fail.

% SVF
generalize_expression(some(P,Y), some(PV,Y),classExpression) :-
        generalize_expression(P,PV,propertyExpression).
generalize_expression(some(P,Y), some(P,YV),classExpression) :-
        generalize_expression(Y,YV,classExpression).

% AND
generalize_expression(and([X|L]), and([X2|L]),classExpression) :-
        generalize_expression(X,X2,classExpression).
generalize_expression(and([X]), and([X2]),classExpression) :-
        generalize_expression(X,X2,classExpression).
generalize_expression(and([X|L]), and([X|L2]),classExpression) :-
        generalize_expression(and(L), and(L2),classExpression).

% VAR
generalize_expression(X, _GenVar, _T) :- atomic(X).

% ========================================
% ALL GENERALIZATIONS
% ========================================

candidate(X, Matches) :-
        setof(GrX,ground_expression(GrX),GrXs),
        member(GrX,GrXs),
        generalize_expression(GrX,X),
        setof(X,member(X,GrXs),Matches).

pattern_matches(X,GrXs,Matches) :-
        setof(X,member(X,GrXs),Matches).

:- dynamic seen/1.
generate_patterns(Opts) :-
        setof(GrX,ground_expression(GrX),GrXs),
        member(GrX,GrXs),
        generalize_expression(GrX,X),
        seed_from(X,1,GrXs,Opts).
generate_patterns(_) :-
        debug(patternizer,'Done!',[]).

seed_from(X,ParentCount,GrXs,Opts) :-
        copy_term(X,X_Unground),
        \+ has_been_seen(X),
        debug(patternizer,'Testing generalized expression: ~q',[X]),
        mark_seen(X),
        \+ exclude(X,Opts),
        pattern_matches(X_Unground,GrXs,Matches),
        length(Matches,Num),
        option(min(Min),Opts,8),
        Num >= Min,
        Num > ParentCount,
        debug(patternizer,'Passed; writing',[]),
        write_candidate(X, Matches, Opts),
        generalize_expression(X_Unground,X2),
        debug(patternizer,'  Generalized further ~q -> ~q',[X_Unground, X2]),
        seed_from(X2,Num,GrXs,Opts),
        fail.

exclude(X,Opts) :-
        option(exclude_prefixes(Prefixes),Opts),
        Prefixes\=[],
        expr_class_signature(X,CSig),
        member(C,CSig),
        member(Prefix,Prefixes),
        (   atom_concat(Prefix,_,C)
        ;   rdf_global_id(Prefix:_,C)).
exclude(X,Opts) :-
        option(trim(true),Opts),
        non_trim(X).


has_been_seen(X) :-
        numbervars(X,0,_),
        seen(X).
mark_seen(X) :-
        numbervars(X,0,_),
        assert(seen(X)).

setup_write(PId, Opts) :-
        option(dir(Dir),Opts),
        !,
        concat_atom([Dir,/,PId,'.yaml'], Path),
        tell(Path).
setup_write(_, _).


write_candidate(X, Matches, Opts) :-
        numbervars(X,0,_),
        expr_atom(X,PName,text/id,_),
        make_id(PName, PId),
        option(base(Base),Opts,'http://purl.obolibrary.org/obo/foo'),
        concat_atom([Base,PId],/,PUrl),
        length(Matches,Num),
        extract_examples(Matches, Examples),
        expr_class_signature(X,CSig),
        expr_property_signature(X,PSig),
        expr_var_signature(X,VSig),
        debug(patternizer,'Class sig: ~q',[CSig]),
        debug(patternizer,'Prop sig: ~q',[PSig]),
        debug(patternizer,'Var sig: ~q',[VSig]),
        maplist([C,obj(C,Id,N)]>>(label_or_frag(C,N),uri_curie(C,Id)),CSig,CSet),
        maplist([C,obj(C,Id,N)]>>(label_or_frag(C,N),uri_curie(C,Id)),PSig,PSet),
        maplist([V,v(VN,V,'owl:Thing')]>>(V='$VAR'(VN1),atom_concat(v,VN1,VN)),VSig,VSet),

        setup_write(PId, Opts),
        
        format('pattern_name: ~w~n',[PName]),
        format('pattern_iri: ~w~n',[PUrl]),
        nl,
        format('description: >-~n'),
        format('  This is auto-generated. Add your description here~n~n'),
        format('  Examples: ~w (~w total)~n',[Examples, Num]),
        nl,
        format('classes:~n'),
        forall(member(obj(C,Id,N),CSet),
               format('  ~w: "~w"~n',[N,Id])),
        nl,
        format('relations:~n'),
        forall(member(obj(C,Id,N),PSet),
               format('  ~w: "~w"~n',[N,Id])),
        nl,
        format('vars:~n'),
        forall(member(v(VN,_,VT),VSet),
               format('  ~w: "~w"~n',[VN,VT])),
        nl,
        expr_atom(X,XName,text/name,NameVars),
        format('name:~n'),
        format('  text: "~w"~n',[XName]),
        show_vars('  ', NameVars, VSet),
        nl,
        expr_atom(X,XDef,text/def,DefVars),
        format('def:~n'),
        format('  text: "~w."~n',[XDef]),
        show_vars('  ', DefVars, VSet),
        nl,
        expr_atom(X,XEquiv,text/owl,EquivVars),
        format('equivalentTo:~n'),
        format('  text: "~w"~n',[XEquiv]),
        show_vars('  ', EquivVars, VSet),
        told,
        mark_seen(X). % <-- NOT NEEDED NOW

show_vars(Indent,OrderedVars,VSet) :-
        write(Indent),
        format('vars:~n'),
        forall((member(V,OrderedVars),member(v(VN,'$VAR'(V),_),VSet)),
               format('~w  - ~w~n',[Indent,VN])).

extract_examples(Matches, ExamplesA) :-
        select_class_from_match_expr(E1,Matches,Matches2),
        select_class_from_match_expr(E2,Matches2,Matches3),
        select_class_from_match_expr(E3,Matches3,_),
        maplist([In,Out]>>(label_or_frag(In,N),sformat(Out,'[~w](~w)',[N,In])),[E1,E2,E3],Examples),
        concat_atom(Examples, ', ', ExamplesA).
select_class_from_match_expr(E,Matches,Matches2) :-
        select(X,Matches,Matches2),
        !,
        ground_expression(E,X).


uri_curie(C,Id) :-
        rdf_global_id(Pre:Local,C),
        concat_atom([Pre,Local],:,Id),
        !.
uri_curie(C,C).

make_id(N,Id) :-
        concat_atom(L,' ',N),
        concat_atom(L,'_',Id).

expr_class_signature(X,L) :- setof(M,expr_references_class(X,M),L).
expr_references_class('$VAR'(_),_) :- fail.
expr_references_class(and(L),C) :- member(M,L),expr_references_class(M,C).
expr_references_class(some(_,A),C) :- expr_references_class(A,C).
expr_references_class(X,X) :- atom(X).


expr_property_signature(X,L) :- setof(M,expr_references_property(X,M),L).
expr_references_property('$VAR'(_),_) :- fail.
expr_references_property(and(L),P) :- member(M,L),expr_references_property(M,P).
expr_references_property(some(P,_),P) :- atom(P),!. % TODO: property expressions
expr_references_property(some(_,X),P) :- expr_references_property(X,P).

expr_var_signature(X,L) :- setof(M,expr_references_var(X,M),L).
expr_references_var(V,V) :- V='$VAR'(_).
expr_references_var(and(L),P) :- member(M,L),expr_references_var(M,P).
expr_references_var(some(P,_),V) :- expr_references_var(P,V).
expr_references_var(some(_,Y),V) :- expr_references_var(Y,V).

non_trim(and(L)) :- \+ member(some(_,_), L).
non_trim(and(L)) :- member(X,L), non_trim(X).
non_trim(some(P,V)) :- is_nvar(P),is_nvar(V).

is_nvar('$VAR'(_)).

label_or_frag(X,N) :- rdfs_label(X,S),!,ensure_atom(S,N).
label_or_frag(X,X).

%! expr_atom(+Expr, ?Atom, +Context, ?Vars)
%
%   generate an atom from an expression, collecting vars used in order
%
expr_atom('$VAR'(V), 'X', _/id, [V]) :- !.
expr_atom('$VAR'(V), '%s', text/_, [V]) :- !.
expr_atom('$VAR'(V), N, _, V) :- !, sformat(N, 'v~w', [V]).

expr_atom(X, N, text/owl, []) :- atomic(X), !, label_or_frag(X, N1), sformat(N, '\'~w\'', [N1]).
expr_atom(X, N, _, []) :- atomic(X), !, label_or_frag(X, N).

expr_atom(and(L), N, Context, Vars):-
        !,
        exprs_atoms(L, L2, Context, Vars),
        serialize_conj(L2, N, Context).
expr_atom(some(P, V), N, Context, Vars):-
        !, 
        expr_atom(P, PN, Context, Vars1), 
        expr_atom(V, VN, Context, Vars2),
        append(Vars1, Vars2, Vars),
        serialize_some(PN, VN, N, Context).

expr_atom(X, N, _, []) :- sformat(N, '??~q', [X]).

exprs_atoms([X], [X2], Context, Vars) :-
        !,
        expr_atom(X, X2, Context, Vars).
exprs_atoms([X|L], [X2|L2], Context, Vars) :-
        !,
        expr_atom(X, X2, Context, Vars1),
        exprs_atoms(L, L2, Context, Vars2),
        append(Vars1, Vars2, Vars).

serialize_conj([X1|L], N, _/def) :-
        !, 
        concat_atom(L, ' and ', LA), 
        sformat(N, 'Any ~w that ~w', [X1, LA]).
serialize_conj(L, N, _/name) :-
        !, 
        concat_atom(L, ' ', N).
serialize_conj(L, N, _/id) :-
        !, 
        concat_atom(L, ' ', N).
serialize_conj(L2, N, _) :-
        concat_atom(L2, ' and ', N).

serialize_some(PN, VN, N, _/name) :-
        !, 
        concat_atom([PN, VN], ' ', N).
serialize_some(PN, VN, N, _/id) :-
        !, 
        concat_atom([PN, VN], ' ', N).
serialize_some(PN, VN, N, _/def) :-
        !, 
        concat_atom([PN, VN], ' a ', N).
serialize_some(PN, VN, N, _) :-
        concat_atom([PN, VN], ' some ', N).




