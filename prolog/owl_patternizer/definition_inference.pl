/** <module> definition_inference

*/
:- module(definition_inference,
          [atom_class_token/4,
           parse_atom/3,
           best_parse_atom/4,
           best_parse_class/4,
           parse_tokens/3,

           % TODO: move
           save_axiom/1,
           save_axiom/2,
           
           assert_inferred_equiv_axioms/2]).

:- use_module(library(owl_patternizer)).
:- use_module(library(rdf_matcher)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(tabling)).

:- table class_prop_tokens/4.

class_prop_tokens(C,P,OpenToks,Rest) :-
        tr_annot(C,P,V,_,_,_),
        concat_atom(Toks,' ',V),
        append(Toks,Rest,OpenToks).

%! atom_class_token(+Atom, ?Cls, ?PreList, ?Rest) is nondet
%
%   true if Atom can be parsed as [PreList, Cls, Rest]
atom_class_token(A,C,PreList,Rest) :-
        mutate(_,_,A,A2),
        concat_atom(Toks,' ',A2),
        append(PreList,SubList,Toks),
        class_prop_tokens(C,_P,SubList,Rest),
        \+ ((PreList=[], Rest=[])).

%! parse_atom(+Cls, +Atom, ?Ms:list) is nondet
%! parse_atom(+Cls, +Atom, ?Ms:list, ?Score) is nondet
%
%   parse Atom, ensure that matched entities are
%   (inferred or asserted) superclasses of Cls
%
parse_atom(C, A, Ms) :-
        mutate(_,_,A,A2),
        concat_atom(Toks,' ',A2),
        parse_tokens(C, Toks, Ms),
        Ms=[_,_|_],
        \+ \+ member(span(_,_,_),Ms).

old___parse_atom(C, A, Ms, Score) :-
        parse_atom(C, A, Ms),
        score_spans(Ms, Score),
        debug(def,'Parse ~w -> ~q // Score: ~w' ,[A, Ms, Score]).

parse_atom(C, A, Ms, Score) :-
        atom_bm(A, BM, _),
        recursive_parse_bm(C, BM, Ms),
        score_spans(Ms, Score).

score_spans(Ms, Score) :-
        findall(C,member(span(C,_,_),Ms), Cs),
        Cs=[_|_],
        length(Cs, NC),
        findall(Genus,member(span(Genus,Genus,_),Ms), Genuses),
        length(Genuses, NG),
        nc_ng_score(NC,NG,BaseScore),
        findall(W,member(word(W),Ms),Ws),
        length(Ws,NumWs),
        Score is BaseScore-NumWs.

nc_ng_score(0,_,0) :- !.
nc_ng_score(1,_,1) :- !.
nc_ng_score(_,0,4) :- !.
nc_ng_score(_,1,8) :- !.
nc_ng_score(_,_,6) :- !.


% greedy
recursive_parse_bm(MainCls, BM, [Span|Ms]) :-
        Match=match(_Score,ParentCls,Expr,P,IxnBM),
        setof(Match,
              bm_class_match(MainCls, BM, Match),
              Pairs),
        !,
        reverse(Pairs,[Match|_]),
        BM2 is BM - IxnBM,
        Span = span(Expr, ParentCls, P),
        recursive_parse_bm(MainCls, BM2, Ms).
recursive_parse_bm(_MainCls, BM, Words) :-
        bm_tokens(BM, Toks),
        findall(word(Tok),member(Tok,Toks),Words).
        
bm_class_match(MainCls, BM, Match) :-
        BM > 0,
        Match=match(Score,ParentCls,Expr,P,IxnBM),
        class_references(MainCls, ParentCls, Expr),
        class_prop_bm(ParentCls,P,_V,ParentBM),
        P\=id,
        P\=uri,
        ParentCls \= MainCls,
        IxnBM is BM /\ ParentBM,
        IxnBM > 0,
        LenIxn is popcount(IxnBM),
        LenPBM is popcount(ParentBM),
        Score is LenIxn / LenPBM,
        %debug(def,' POSSIBLEMATCH(~w <=> ~w) = Score: ~w // p=~w // ~w' ,[MainCls, ParentCls, Score, P, _V]),
        Score > 0.66.

        % prioritize anon-expressions
        %(   ParentCls=Expr
        %->  Score = Score1
        %;   Score = Score1),
        %bm_resnik(BM,ParentBM,0,Score),

%%        debug(def,' MATCH(~w <=> ~w) = Score: ~w // p=~w // ~w' ,[MainCls, ParentCls, Score, P, _V]).




        
        
        
%! best_parse_atom(+Cls, +Atom, ?Ms:list, ?Score) is det
%
%   parse Atom, ensure that matched entities are
%   (inferred or asserted) superclasses of Cls
%
best_parse_atom(C, A, Ms, Score) :-
        aggregate(max(Score1,Ms1),parse_atom(C, A, Ms1, Score1),max(Score,Ms)).



%! parse_tokens(+MainClass, +Tokens:list, ?Matches:list) is nondet
%
%   parse a list of word tokens to a match list
%
%   all matched entities must be a superclass of MainClass
%
%     Match = word(Token) | span(ClassExpression, Class, Prop)
%
:- table parse_tokens/3.

parse_tokens(_, [], []).
parse_tokens(MainClass, Tokens, [span(Expr,C,P)|Matches]) :-
        Tokens \= [],
        class_prop_tokens(C,P,Tokens,Rest),
        C\=MainClass,
        class_references(MainClass, C, Expr),
        parse_tokens(MainClass, Rest, Matches).
parse_tokens(MainClass, [T|Tokens], [word(T)|Matches]) :-
        parse_tokens(MainClass, Tokens, Matches).

%! parse_class(+Cls, ?Prop, ?Ms:list, ?Score) is nondet
%
%   parse the label or other annotations for Cls
%
parse_class(C, P, Ms, Score) :-
        tr_annot(C,P,V,_,_,_),
        P\=id,
        P\=uri,
        parse_atom(C,V,Ms,Score).

best_parse_class(C, P, Ms, Score) :-
        aggregate(max(Score1,P-Ms1),parse_class(C, P, Ms1, Score1),max(Score,P-Ms)).


matches_to_class_expression(Ms, and(Xs)) :-
        setof(X,C1^P1^member(span(X,C1,P1),Ms),Xs),
        % at least two members
        Xs=[_,_|_].


% TODO: move these
save_axiom(Axiom) :-
        rdf_default_graph(G),
        save_axiom(Axiom,G).

save_axiom(Axiom,G) :-
        save_axiom(Axiom,_,G).
save_axiom(equivalentTo(A,B),rdf(A,owl:equivalentClass,B),G) :-
        !,
        debug(def, 'Saving ~w = ~w to ~w',[A,B,G]),
        save_expression(A,Ax,G),
        save_expression(B,Bx,G),
        rdf_assert(Ax,owl:equivalentClass,Bx,G).
save_axiom(subClassOf(A,B),rdf(A,rdfs:subClassOf,B),G) :-
        !,
        save_expression(A,Ax,G),
        save_expression(B,Bx,G),
        rdf_assert(Ax,rdfs:subClassOf,Bx,G).
save_axiom(T,T,G) :-
        T=rdf(S,P,O),
        !,
        rdf_assert(S,P,O,G).

save_axiom_with_anns(Axiom,G,Anns) :-
        save_axiom_with_anns(Axiom,_,G,Anns).
save_axiom_with_anns(Axiom,T,G,Anns) :-
        save_axiom(Axiom,T,G),
        T=rdf(S,P,O),
        save_expression(P,Px,G),
        save_expression(S,Sx,G),
        save_expression(O,Ox,G),
        rdf_create_bnode(AxiomNode),
        debug(def, 'Saving AxiomAnnotation ~w = ~w -> ~w // ~w,~w,~w',[AxiomNode,AP,V,S,Px,O]),
        rdf_assert(AxiomNode,owl:annotatedSource,Sx,G),
        rdf_assert(AxiomNode,owl:annotatedProperty,Px,G),
        rdf_assert(AxiomNode,owl:annotatedTarget,Ox,G),
        rdf_assert(AxiomNode,rdf:type,owl:'Axiom',G),
        forall(member(annotation(AP,V),Anns),
               rdf_assert(AxiomNode,AP,V,G)).



% TODO: axiom annotation
save_expression(and(Xs),Node,G) :-
        !,
        rdf_create_bnode(Node),
        debug(def,'Made IXN bnode: ~w',[Node]),
        rdf_assert(Node,rdf:type,owl:'Class',G),
        maplist({G}/[In,Out]>>save_expression(In,Out,G),Xs,IxnNodesPL),
        debug(def, 'AND list = ~w',[IxnNodesPL]),
        rdf_assert_list(IxnNodesPL,IxnNode,G),
        rdf_assert(Node,owl:intersectionOf,IxnNode,G).
save_expression(some(P,V),Node,G) :-
        !,
        rdf_create_bnode(Node),
        save_expression(P,Px,G),
        save_expression(V,Vx,G),
        rdf_assert(Node,rdf:type,owl:'Restriction',G),
        rdf_assert(Node,owl:onProperty,Px,G),
        rdf_assert(Node,owl:someValuesFrom,Vx,G).
save_expression(Node,Node,_) :-
        Node = _^^_,
        !.
save_expression(Node,Node,_) :-
        Node = _@_,
        !.
save_expression(P:X,Node,_) :-
        rdf_global_id(P:X,Node),
        !.
save_expression(Node,Node,_) :-
        atomic(Node),
        !.

        
assert_inferred_equiv_axioms(C,G) :-
        forall((best_parse_class(C, _P, Ms, Score),
                matches_to_class_expression(Ms, Expr),
                rdf_canonical_literal(Score,ScoreLiteral)),
               save_axiom_with_anns(equivalentTo(C,Expr),G,
                                    [annotation(rdfs:score,ScoreLiteral),
                                     annotation(rdfs:comment, "Autogenerated")])).
        



        
:- table class_references/3.
class_references(C,P,X) :- rdfs_subclass_of(C,D), class_directly_references(D,P,X).
class_references(C,P,X) :- class_directly_references(C,P,X).

class_directly_references(C,C,C).
class_directly_references(C,P,some(Rel,P)) :- owl_some(C,Rel,P).



        
