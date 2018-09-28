/**

  generate patterns from an OWL Ontology

  See README for background
  
*/

:- module(owl_patternizer,
          [ground_expression/1,
           generate_patterns/1,
           induce_annotation_pattern/4,
           induce_annotation_pattern_with_freq/5,
           induce_best_annotation_pattern/5,
           unify/3,
           infer_pattern_var_range/3,
           infer_pattern_var_range/4,

           load_import_closure/0]).

:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(tabling)).

% ========================================
% MAP RDF TO PROLOG TERMS
% ========================================

% note: this could be moved to more general-purpose library
%
% currently only a simple EL-subset is supported, but this
% would be trivial to extend
%
% type:
% Expression = NamedObject | and(ExpressionList) | some(Property, Expression)
%
% e.g. and(foo,some(part_of,bar))

:- table class_equiv_expression/2.
class_equiv_expression(C,X) :-
        rdf(C,owl:equivalentClass,Node,G),
        core_ontology(G),
        \+ rdf_is_bnode(C),
        rdf_is_bnode(Node),
        node_expression(Node,X).

:- table ground_expression/1.
ground_expression(X) :-
        class_equiv_expression(_, X).

node_expression(Node,Node) :-
        \+ rdf_is_bnode(Node).

node_expression(Node,and(Xs)) :-
        rdf(Node,owl:intersectionOf,L),
        !,
        setof(X,rdflist_member_expression(L,X),Xs).
node_expression(Node,or(Xs)) :-
        rdf(Node,owl:unionOf,L),
        !,
        setof(X,rdflist_member_expression(L,X),Xs).

node_expression(Node,not(V)) :-
        rdf(Node,owl:complementOf,VNode),
        !,
        node_expression(VNode,V).

node_expression(Node,some(P,V)) :-
        rdf(Node,owl:onProperty,PNode),
        rdf(Node,owl:someValuesFrom,VNode),
        !,
        node_expression(PNode,P),
        node_expression(VNode,V).

node_expression(Node,only(P,V)) :-
        rdf(Node,owl:onProperty,PNode),
        rdf(Node,owl:allValuesFrom,VNode),
        !,
        node_expression(PNode,P),
        node_expression(VNode,V).

rdflist_member_expression(L,X) :-
        rdflist_member(L,M),
        node_expression(M,X).

% ========================================
% GENERALIZING EXPRESSIONS
% ========================================

%! generalize_expression(+Input, ?Generalized, +Type) is nondet
%
%    given any ground or partially ground class expression, find a
%    generalization in which any atomic object (currently class or
%    property) is replaced by a prolog var
%
%    The core data structure here is the same as above, but
%    allows for vars
% 
%    E.g 
%       Expression = NamedClass | Var | and(ExpressionList) | some(Property, Expression)
%

generalize_expression(X,X2) :- generalize_expression(X,X2, classExpression).


generalize_expression(V, _, _) :- var(V),!,fail.

% SOME
generalize_expression(some(P,Y), some(PV,Y),classExpression) :-
        generalize_expression(P,PV,propertyExpression).
generalize_expression(some(P,Y), some(P,YV),classExpression) :-
        generalize_expression(Y,YV,classExpression).

% ONLY
generalize_expression(only(P,Y), only(PV,Y),classExpression) :-
        generalize_expression(P,PV,propertyExpression).
generalize_expression(only(P,Y), only(P,YV),classExpression) :-
        generalize_expression(Y,YV,classExpression).

% NOT
generalize_expression(not(Y), not(YV),classExpression) :-
        generalize_expression(Y,YV,classExpression).

% AND
generalize_expression(and([X|L]), and([X2|L]),classExpression) :-
        generalize_expression(X,X2,classExpression).
generalize_expression(and([X]), and([X2]),classExpression) :-
        generalize_expression(X,X2,classExpression).
generalize_expression(and([X|L]), and([X|L2]),classExpression) :-
        generalize_expression(and(L), and(L2),classExpression).

% OR
generalize_expression(or([X|L]), or([X2|L]),classExpression) :-
        generalize_expression(X,X2,classExpression).
generalize_expression(or([X]), or([X2]),classExpression) :-
        generalize_expression(X,X2,classExpression).
generalize_expression(or([X|L]), or([X|L2]),classExpression) :-
        generalize_expression(or(L), or(L2),classExpression).

% VAR
generalize_expression(X, _GenVar, _T) :- atomic(X).

% ========================================
% GENERATE PATTERNS FOR GENERALIZED EXPRESSIONS
% ========================================

%! generate_patterns(+Options) is det.
%
%   generate candidate patterns and write these to disk.
%
%   TODO: generate a structure and write later rather than coupling
%   generation and I/O
generate_patterns(Opts) :-
        % refresh tables and retract dynamic preds
        abolish_all_tables,
        retractall(seen/1),
        retractall(core_ontology/1),
        retractall(ont_import_loaded/1),
        option(load_import_closure(IsLoadIC),Opts,true),
        load_import_closure(IsLoadIC),
        % start with all ground expressions;
        % these will be generalized one 'hop'
        setof(GrX,ground_expression(GrX),GrXs),
        member(GrX,GrXs),
        generalize_expression(GrX,X),
        % start with one level of generatization
        generate_patterns_from_seed(X,1,GrXs,Opts).
generate_patterns(_) :-
        debug(patternizer,'Done!',[]).

:- dynamic seen/1.
% generate and write patterns, start from seed pattern X
% recursively generalize and write.
generate_patterns_from_seed(X,ParentCount,GrXs,Opts) :-
        copy_term(X,X_Unground),
        \+ has_been_seen(X),
        debug(patternizer,'Testing generalized expression: ~k',[X]),
        mark_seen(X),
        debug(patternizer,'Exclusion test: ~k // ~q',[X,Opts]),
        \+ exclude(X,Opts),
        setof(X_Unground,member(X_Unground,GrXs),Matches),
        length(Matches,Num),
        debug(patternizer,' Matches: ~w',[Num]),        
        option(min(Min),Opts,8),
        (   Num >= Min,
            Num > ParentCount
        ->  debug(patternizer,'Passed; writing',[]),
            write_candidate(X, Matches, Opts)
        ;   debug(patternizer,'Failed; skipping',[])),
        generalize_expression(X_Unground,X2),
        debug(patternizer,'  Generalized further ~k ====> ~k',[X_Unground, X2]),
        generate_patterns_from_seed(X2,Num,GrXs,Opts),
        fail.

%! exclude(+Pattern, +Opts:list) is semidet
%
%   true if Pattern should be excluded 
%
exclude(X,Opts) :-
        expr_class_signature(X,CSig),
        length(CSig,Len),
        option(max_class_signature(Max),Opts,5),
        Len > Max.
exclude(X,Opts) :-
        option(generalize_properties(false),Opts,true),
        has_var_prop(X).
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
        non_trim(X),
        !.
exclude(X,Opts) :-
        % some ontologies (e.g. CL) have long intersection lists
        % our current strategy does not allow effective generalization over
        % these so we skip
        option(max_and_cardinality(Limit),Opts,4),
        exceeds_max_and_cardinality(X,Limit),
        !.

% rules to categorize some expressions as non-"trim"
non_trim(and(L)) :- \+ member(some(_,_), L).  % ANDs must have at least one SVF (obo-esque hardcoding)
non_trim(and(L)) :- member(X,L), non_trim(X). % descend
non_trim(some(P,V)) :- is_nvar(P),is_nvar(V). % SVF must have one instantiated position
non_trim(some(_,X)) :- non_trim(X).

% test if pattern contains a variable in property position
has_var_prop(and(L)) :- member(X,L), has_var_prop(X). % descend
has_var_prop(or(L))  :- member(X,L), has_var_prop(X). % descend
has_var_prop(not(X))  :- has_var_prop(X). % descend
has_var_prop(some(P,_)) :- is_nvar(P).
has_var_prop(some(_,X)) :- has_var_prop(X).

is_nvar('$VAR'(_)).


exceeds_max_and_cardinality(and(L),Limit) :- length(L,Len), Len > Limit, !.
exceeds_max_and_cardinality(and(L),Limit) :- member(X,L), exceeds_max_and_cardinality(X,Limit).
exceeds_max_and_cardinality(or(L),Limit) :- length(L,Len), Len > Limit, !.
exceeds_max_and_cardinality(or(L),Limit) :- member(X,L), exceeds_max_and_cardinality(X,Limit).
exceeds_max_and_cardinality(some(_,X),Limit) :- exceeds_max_and_cardinality(X,Limit).
exceeds_max_and_cardinality(not(X),Limit) :- exceeds_max_and_cardinality(X,Limit).

has_been_seen(X) :-
        numbervars(X,0,_),
        seen(X).
mark_seen(X) :-
        numbervars(X,0,_),
        assert(seen(X)).

setup_write(PId, Opts) :-
        option(dir(Dir),Opts),
        !,
        make_directory_path(Dir),
        concat_atom([Dir,/,PId,'.yaml'], Path),
        tell(Path).
setup_write(_, _).

%! write_candidate(+X, +Matches:list, +Opts:list) is det
%
%    writes a candidate pattern expression X to disk
%
%    assumes matching ground class expressions Matches have been pre-computed (for efficiency)
%
%    the pattern expression X is any class expression as defined above, but allowing for
%    variables are represented as ground prolog terms of the form '$VAR'(N)
%
%    TODO: seperate out pattern structure generation from I/O
write_candidate(X, Matches, Opts) :-
        debug(patternizer,'Writing ~w',[X]),
        % translate prolog vars into terms of the form $VAR(N)
        numbervars(X,0,_),
        
        % generate a name and id/uri for this pattern (TODO: improve)
        expr_atom(X,PName,text/id,_),
        make_id(PName, PId),
        debug(patternizer,'ID = ~w',[PId]),
        option(base(Base),Opts,'http://purl.obolibrary.org/obo/foo'),
        concat_atom([Base,PId],/,PUrl),

        % setup
        length(Matches,Num),
        extract_examples(Matches, Examples),

        % get signatures
        expr_class_signature(X,CSig),
        expr_property_signature(X,PSig),
        expr_var_signature(X,VSig),
        setof(RangeCls,V^RangeCls^(infer_pattern_var_range(X,Matches,V,RangeClsSet),member(RangeCls,RangeClsSet)),RangeSig),
        ord_union(CSig, RangeSig, AllCls),
        debug(patternizer,'Class sig: ~q + ~w',[CSig, RangeSig]),
        debug(patternizer,'Prop sig: ~q',[PSig]),
        debug(patternizer,'Var sig: ~q',[VSig]),

        % mapping of short labels to URIs for all signatures
        maplist([C,obj(C,Id,N)]>>(label_or_frag(C,N),uri_curie(C,Id)),AllCls,CSet),
        maplist([C,obj(C,Id,N)]>>(label_or_frag(C,N),uri_curie(C,Id)),PSig,PSet),
        maplist({X,Matches}/[V,v(VN,V,VRangeAtom)]>>(V='$VAR'(VN1),
                                     infer_pattern_var_range(X,Matches,VN1,VRangeDisj),
                                     uris_as_disjunction_expr(VRangeDisj, VRangeAtom),
                                     atom_concat(v,VN1,VN)),
                VSig,VSet),
        
        % assume OBO definition prop by default
        option(def_prop(DefAP),Opts,'http://purl.obolibrary.org/obo/IAO_0000115'),
        option(name_prop(NameAP),Opts,rdfs:label),
        option(annotations(Anns),Opts,[]),

        % hacky: use tell/1
        setup_write(PId, Opts),

        % hacky write to YAML
        format('# options: ~q~n',[Opts]),
        format('pattern_name: ~w~n',[PName]),
        format('pattern_iri: ~w~n',[PUrl]),
        nl,
        format('description: >-~n'),
        format('  This is auto-generated. Add your description here~n~n'),
        format('  Examples: ~w (~w total)~n',[Examples, Num]),
        nl,
        format('classes: '),show_obj_list('  ',CSet),
        nl,
        format('relations: '),show_obj_list('  ',PSet),
        nl,
        format('vars:~n'),
        forall(member(v(VN,_,VT),VSet),
               format('  ~w: "~w"~n',[VN,VT])),
        nl,
        show_induced_textobj(name, X, Matches, NameAP, VSet),
        nl,
        show_induced_textobj(def, X, Matches, DefAP, VSet),
        nl,
        show_induced_anns(Anns, X, Matches, VSet),
        nl,
        expr_atom(X,XEquiv,text/owl,EquivVars),
        format('equivalentTo:~n'),
        format('  text: "~w"~n',[XEquiv]),
        show_vars('  ', EquivVars, VSet),
        told.

% TODO: escape quotes
uris_as_disjunction_expr(Cs, A) :-
        findall(CA,
                (   member(C,Cs),
                    label_or_frag(C,N),
                    sformat(CA,'\'~w\'',[N])),
                CAs),
        concat_atom(CAs,' or ', A).


%! show_induced_textobj(+Tag, +X, +Matches:list, +AProp, +VSet:list) is det.
%
%   writes text objects
%
%   in dosdp, text objects describe how
%   literal text is to be generated for template values
%
%   here we induce from lexical patterns in existing ontology
show_induced_textobj(Tag, X, Matches, AProp, VSet) :-
        format('~w:~n', [Tag]),
        rdf_global_id(AProp, APropURI),
        (   induce_best_annotation_pattern(X, Matches, APropURI, fmt(FmtAtom,Vars), Freq),
            % TODO: this is completely ad-hoc
            % re-calculate exact count from frequency
            % frequency must be above set threshold
            % AND there must be >1 to prevent massive overfitting
            % UNLESS there is so few examples, in which case we
            % pick an exemplar (this is a hack to work for the pizza ontology)
            length(Matches,Total),
            Num is Freq * Total,
            (   Total > 10
            ->  Num >= 2
            ;   Num >= 1)
        ->  format('  # Induced, frequency=~w, p=~w ~n',[Freq, APropURI])
        ;   expr_atom(X,FmtAtom,text/Tag,Vars),
            format('  # Could not induce ~w, using default~n',[Tag])),
        format('  text: "~w"~n',[FmtAtom]),
        show_vars('  ', Vars, VSet).

show_induced_anns([], _, _, _) :- !.
show_induced_anns(Anns, X, Matches, VSet) :-
        format('annotationProperties:~n'),
        forall(member(ann(N,Id,_), Anns),
               format('  ~w: "~w"~n',[N,Id])),
        nl,
        (   setof(P-FmtObj, induced_ann_textobj(Anns, X, Matches, P, FmtObj), Pairs)
        ->  format('annotations:~n'),
            show_ann_textobjs(Pairs, VSet, [])
        ;   format('# could not infer annotations~n')),
        nl.

induced_ann_textobj(Anns, X, Matches, N, FmtObj) :-
        member(ann(N,PId,MinFreq), Anns),
        rdf_global_id(PId, PURI),
        induce_best_annotation_pattern(X, Matches, PURI, FmtObj, Freq),
        length(Matches,Total),
        Num is Freq * Total,
        Num >= 2,
        Freq >= MinFreq.

show_ann_textobjs([], _, _).
show_ann_textobjs([Pair|Pairs], VSet, Done) :-
        Pair=_-FmtObj,
        member(FmtObj,Done),
        !,
        show_ann_textobjs(Pairs, VSet, Done).
show_ann_textobjs([Pair|Pairs], VSet, Done) :-
        Pair=P-FmtObj,
        !,
        FmtObj=fmt(FmtAtom,Vars),
        format('  - annotationProperty: ~w~n',[P]),
        format('    # Induced p=~w ~n',[P]),
        format('    text: "~w"~n',[FmtAtom]),
        show_vars('    ', Vars, VSet),
        show_ann_textobjs(Pairs, VSet, [FmtObj|Done]).

% utils for writing signatures in yaml
show_obj_list(_, []) :-
        !,
        writeln('[]').

show_obj_list(Indent, CSet) :-
        nl,
        forall(member(obj(_C,Id,N),CSet),
               format('~w~w: "~w"~n',[Indent,N,Id])),
        nl.

show_vars(Indent,OrderedVars,VSet) :-
        write(Indent),
        format('vars:~n'),
        forall((member(V,OrderedVars),member(v(VN,'$VAR'(V),_),VSet)),
               format('~w  - ~w~n',[Indent,VN])).

% sample first 3 classes that instantiate a pattern as exemplars
% TODO: this has the effect of selecting "samey" classes, vary this somehow
extract_examples(Matches, ExamplesA) :-
        select_first_n(Matches, 3, Sample),
        maplist([Match,Out]>>(class_equiv_expression(Id,Match),label_or_frag(Id,N),sformat(Out,'[~w](~w)',[N,Id])),Sample,Examples),
        concat_atom(Examples, ', ', ExamplesA).

%select_class_from_match_expr(E,Matches,Matches2) :-
%        select(X,Matches,Matches2),
%        !,
%        class_equiv_expression(E,X).

select_first_n(_,0,[]) :- !.
select_first_n([],_,[]) :- !.
select_first_n([H|List],N,[H|Sublist]) :-
        Nminus1 is N-1,
        select_first_n(List,Nminus1,Sublist).


        

% ========================================
% OWL UTILS
% ========================================

% TODO: use generic visitor pattern

% get class signature of an expression
expr_class_signature(X,L) :- solutions(M,expr_references_class(X,M),L).
expr_references_class('$VAR'(_),_) :- fail.
expr_references_class(and(L),C) :- member(M,L),expr_references_class(M,C).
expr_references_class(or(L),C) :-  member(M,L),expr_references_class(M,C).
expr_references_class(not(A),C) :- expr_references_class(A,C).
expr_references_class(some(_,A),C) :- expr_references_class(A,C).
expr_references_class(only(_,A),C) :- expr_references_class(A,C).
expr_references_class(X,X) :- atom(X).

% get property signature of an expression
expr_property_signature(X,L) :- solutions(M,expr_references_property(X,M),L).
expr_references_property('$VAR'(_),_) :- fail.
expr_references_property(and(L),P) :- member(M,L),expr_references_property(M,P).
expr_references_property(or(L),P) :-  member(M,L),expr_references_property(M,P).
expr_references_property(some(P,_),P) :- atom(P),!. % TODO: property expressions
expr_references_property(only(P,_),P) :- atom(P),!. % TODO: property expressions
expr_references_property(some(_,X),P) :- expr_references_property(X,P).
expr_references_property(only(_,X),P) :- expr_references_property(X,P).
expr_references_property(not(X),P) :- expr_references_property(X,P).

% get variable signature of an expression
expr_var_signature(X,L) :- setof(M,expr_references_var(X,M),L).
expr_references_var(V,V) :- V='$VAR'(_).
expr_references_var(and(L),P) :- member(M,L),expr_references_var(M,P).
expr_references_var(or(L),P) :- member(M,L),expr_references_var(M,P).
expr_references_var(some(P,_),V) :- expr_references_var(P,V).
expr_references_var(only(P,_),V) :- expr_references_var(P,V).
expr_references_var(some(_,Y),V) :- expr_references_var(Y,V).
expr_references_var(only(_,Y),V) :- expr_references_var(Y,V).
expr_references_var(not(Y),V) :- expr_references_var(Y,V).

% ========================================
% SERIALIZATION OF OWL EXPRESSIONS TO TEXT
% ========================================

%! expr_atom(+Expr, ?Atom, +Context, ?Vars)
%
%   generate an atom from an expression, collecting vars used in order
%
%   Context = Type/Tag
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
expr_atom(or(L), N, Context, Vars):-
        !,
        exprs_atoms(L, L2, Context, Vars),
        serialize_disj(L2, N, Context).
expr_atom(some(P, V), N, Context, Vars):-
        !, 
        expr_atom(P, PN, Context, Vars1), 
        expr_atom(V, VN, Context, Vars2),
        append(Vars1, Vars2, Vars),
        serialize_some(PN, VN, N, Context).
expr_atom(only(P, V), N, Context, Vars):-
        !, 
        expr_atom(P, PN, Context, Vars1), 
        expr_atom(V, VN, Context, Vars2),
        append(Vars1, Vars2, Vars),
        serialize_only(PN, VN, N, Context).
expr_atom(not(V), N, Context, Vars):-
        !, 
        expr_atom(V, VN, Context, Vars),
        serialize_not(VN, N, Context).

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

serialize_disj([X1|L], N, _/def) :-
        !, 
        concat_atom(L, ' or ', LA), 
        sformat(N, 'Any ~w that ~w', [X1, LA]).
serialize_disj(L, N, _/name) :-
        !, 
        concat_atom(L, '/', N).
serialize_disj(L, N, _/id) :-
        !, 
        concat_atom(L, '-or-', N).
serialize_disj(L2, N, _) :-
        concat_atom(L2, ' or ', N).

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
        sformat(N,'(~w some ~w)',[PN, VN]).
serialize_not(VN, N, _) :-
        sformat(N,'(not ~w)',[VN]).

serialize_only(PN, VN, N, _/name) :-
        !, 
        concat_atom([PN, VN], ' ', N).
serialize_only(PN, VN, N, _/id) :-
        !, 
        concat_atom([PN, VN], ' ', N).
serialize_only(PN, VN, N, _/def) :-
        !, 
        concat_atom([PN, VN], ' a ', N).
serialize_only(PN, VN, N, _) :-
        sformat(N,'(~w only ~w)',[PN, VN]).
        %concat_atom([PN, VN], ' some ', N).

solutions(T,G,L) :- setof(T,G,L),!.
solutions(_,_,[]).

% deterministically map a URI to a single label, or the URI frag
label_or_frag(X,N) :- rdfs_label(X,S),!,ensure_atom(S,N).
label_or_frag(X,X).

% ========================================
% UNIFICATION
% ========================================

% note: prolog has built-in unification
% however, this is order-sensitive for lists
% so we roll our own here;
% we also use the numervalsified term

%! unify(+Query, +Target, ?Bindings) is nondet.

unify(Q, T, Bs) :-
        unify1(Q, T, Bs1),
        sort(Bs1, Bs).
unify1(X, X, []).
unify1(Query, '$VAR'(N), [N=Query]) :- atomic(Query).
unify1(not(QV), not(TV), Bindings) :-
        unify1(QV,TV,Bindings).
unify1(some(QP,QV), some(TP,TV), Bindings) :-
        unify1(QP,TP,Bindings1),
        unify1(QV,TV,Bindings2),
        append(Bindings1, Bindings2, Bindings).
unify1(only(QP,QV), only(TP,TV), Bindings) :-
        unify1(QP,TP,Bindings1),
        unify1(QV,TV,Bindings2),
        append(Bindings1, Bindings2, Bindings).
unify1(and([Q|QL]), and(TL), Bindings) :-
        % use select/3 in order to maintain order-independence
        select(T,TL,TL2),
        unify1(Q,T,Bindings1),
        unify1(and(QL), and(TL2), Bindings2),
        append(Bindings1, Bindings2, Bindings).
unify1(or([Q|QL]), or(TL), Bindings) :-
        % use select/3 in order to maintain order-independence
        select(T,TL,TL2),
        unify1(Q,T,Bindings1),
        unify1(or(QL), or(TL2), Bindings2),
        append(Bindings1, Bindings2, Bindings).

% ========================================
% INFER VAR RANGE
% ========================================

expression_var_value(X, Matches, V, Obj) :-
        expression_var_value(X, Matches, _, V, Obj).

expression_var_value(X, Matches, M, V, Obj) :-
        member(M,Matches),
        unify(M,X,Bindings),
        member(V=Obj, Bindings).

infer_pattern_var_range(X, V, Range) :-
        pattern_matching_ground_expressions(X, Matches),
        infer_pattern_var_range(X, Matches, V, Range).

infer_pattern_var_range(X, Matches, V, Range) :-
        setof(Obj,expression_var_value(X, Matches, V, Obj), Objs),
        setwise_mrca(Objs, Range).

        

% reflecive ancestors excluding owl:Thing
anc(Obj,Anc) :-      rdfs_subclass_of(Obj,Anc), \+rdf_global_id(owl:'Thing', Anc).
anc(Obj,Obj).
ancs(Obj,Ancs) :- setof(A,anc(Obj,A),Ancs).

setwise_mrca([Obj|Objs], Range) :-
        ancs(Obj,Ancs),
        setwise_mrca(Objs, Ancs, Range),
        !.
setwise_mrca(Objs, Range) :-
        setof(A,Obj^(member(Obj,Objs),anc(Obj,A)),Ancs),
        nr_subset(Ancs,Range),
        length(Range,Len),
        Len < 20,
        !.
setwise_mrca(_, ['owl:Thing']).



setwise_mrca([], Ancs, Range) :-
        nr_subset(Ancs, Range).
setwise_mrca([Obj|Objs], AncsIn, Range) :-
        ancs(Obj,Ancs),
        ord_intersection(Ancs, AncsIn, AncsOut),
        setwise_mrca(Objs, AncsOut, Range).

nr_subset(Objs, NrSet) :-
        setof(Obj,(member(Obj,Objs),\+ redundant_with(Obj, Objs)), NrSet).

redundant_with(Obj, Objs) :-
        member(Obj2, Objs),
        Obj \= Obj2,
        anc(Obj2, Obj).
        


% ========================================
% LEXICAL INDUCTION
% ========================================
% given existing annotation assertions in ontology (label, def, etc)
% infer the text object in a dosdp, assuming the class has a logical def

%! induce_annotation_pattern(+ExprPattern, +Matches:list, ?Prop, ?Cls, ?FmtObj) is nondet
%! induce_annotation_pattern(+ExprPattern, ?Matches:list, ?Prop, ?Cls, ?FmtObj) is nondet
%
% e.g. and($1 some(part_of $2)) => fmt('%s of %s',[$1, $2])
%
% FmtObj = fmt(Fmt, Vars)
%
induce_annotation_pattern(XV, Matches, Prop, fmt(Fmt,Vars)) :-
        induce_annotation_pattern(XV, Matches, Prop, _, fmt(Fmt,Vars)).

induce_annotation_pattern(XV, Matches, Prop, C, fmt(Fmt,Vars)) :-
        pattern_matching_ground_expressions(XV, Matches),
        %(   nonvar(Matches)
        %->  member(XG,Matches)
        %;   true),
        class_equiv_expression(C,XG),
        unify(XG,XV,Bindings),
        rdf(C,Prop,Literal),
        rdf_is_literal(Literal),
        ensure_atom(Literal,PropVal),
        tokenize_by_bindings(PropVal, Bindings, Spans1),
        sort(Spans1,Spans),
        debug(patternizer,'Cls: ~w ~w Spans: ~q // expr=~q',[C, Prop, Spans, XV]),
        slice_atom_by_spans(PropVal, Spans, 0, Vars, Toks),
        concat_atom(Toks, '%s', Fmt).

induce_annotation_pattern_with_freq(XV, Matches, Prop, Fmt, Count/Total) :-
        pattern_matching_ground_expressions(XV, Matches),
        length(Matches, Total),
        aggregate(count, C, Matches^induce_annotation_pattern(XV,Matches,Prop,C,Fmt), Count).

induce_best_annotation_pattern(XV, Matches, Prop, Fmt, Freq) :-
        aggregate(max(Freq,Fmt), induce_annotation_pattern_with_freq(XV, Matches, Prop, Fmt, Freq), max(Freq, Fmt)).


% find spans matching labels for each variable
tokenize_by_bindings(Input, [Var=Entity|Bindings], [span(Start,Len,Prop,Var)|Tokens]) :-
        rdf(Entity, Prop, LitVal),
        ensure_atom(LitVal,AtomVal),
        sub_atom_ci(Input, Start, Len, _, AtomVal),
        tokenize_by_bindings(Input, Bindings, Tokens).
tokenize_by_bindings(_, [], []).

% create textual 'introns'
slice_atom_by_spans(Atom, [span(Start, Len, _Prop, Var) | Spans], Offset, [Var|Vars], [Tok|Toks]) :-
        IntronLen is Start-Offset,
        % disallow overlapping spans
        IntronLen >= 0,
        sub_atom_ci(Atom, Offset, IntronLen, _, Tok),
        Offset2 is Start+Len,
        slice_atom_by_spans(Atom, Spans, Offset2, Vars, Toks).
slice_atom_by_spans(Atom, [], Offset, [], [Tok]) :-
        sub_atom_ci(Atom, Offset, _, 0, Tok).

% convenience util; get matching ground expressions if not set already
pattern_matching_ground_expressions(_, Matches) :-
        nonvar(Matches),
        !.
pattern_matching_ground_expressions(XV, Matches) :-
        setof(Match,pattern_matching_ground_expression(XV,Match),Matches).
pattern_matching_ground_expression(XV, X) :-
        ground_expression(X),
        unify(X,XV,_).

% ========================================
% RDF UTILS
% ========================================
% these may move

uri_curie(C,Id) :-
        rdf_global_id(Pre:Local,C),
        concat_atom([Pre,Local],:,Id),
        !.
uri_curie(C,C).

make_id(N,Id) :-
        atom_chars(N,Chars),
        maplist([In,Out]>>safe_char(In,Out),Chars,Chars2),
        atom_chars(Id,Chars2).

sub_atom_ci(A,S,L,R,Sub) :-
        nonvar(Sub),
        downcase_atom(A,A2),
        downcase_atom(Sub,Sub2),
        sub_atom(A2,S,L,R,Sub2).

sub_atom_ci(A,S,L,R,Sub) :-
        sub_atom(A,S,L,R,Sub).

          
:- dynamic core_ontology/1.
:- dynamic ont_import_loaded/1.
load_import_closure :-
        load_import_closure(true).
load_import_closure(IsLoadIC) :-
        % TODO: move this section elsewhere
        setof(G,S^P^O^rdf(S,P,O,G),Graphs),
        forall(member(G,Graphs),
               assert(core_ontology(G))),
        IsLoadIC,
        rdf(_,owl:imports,Ont),
        \+ ont_import_loaded(Ont),
        debug(patternizer,'Loading: ~w',[Ont]),
        rdf_load(Ont),
        assert(ont_import_loaded(Ont)),
        fail.
load_import_closure(_).

safe_char(X,X) :- X @>= 'a', X @=< 'z',!.
safe_char(X,X) :- X @>= 'A', X @=< 'Z',!.
safe_char(X,X) :- X @>= '0', X @=< '9',!.
safe_char(_,'_').

