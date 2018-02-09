:- module(edcgs, [
      edcg_rules_to_dcg_rules/0,
      edcg_rule_to_dcg_rule/2,
      sequence/5
   ]).

:- op(1200, xfx, ==>).

:- dynamic user:edcgs_expansion_mode/1.
expansion_mode(Mode) :-
   user:edcgs_expansion_mode(Mode),
   !.
expansion_mode(list).


/* edcg_rules_to_dcg_rules <-
      */

edcg_rules_to_dcg_rules :-
   forall( X1==>Y1,
      ( edcg_rule_to_dcg_rule(X1==>Y1, X2-->Y2),
        expand_term(X2-->Y2, Rule),
        assert(Rule) ) ).


/* edcg_rule_to_dcg_rule(X1==>Y1, X2-->Y2) <-
      */

edcg_rule_to_dcg_rule(X1==>Y1, X2-->Y2) :-
   edcg_formula_to_dcg_formula(Y1, Y2, Args),
   X1 =.. As1,
   ( expansion_mode(list),
      As1 = [H|_],
      Res =.. [H,Args],
      append(As1, [Res], As2)
   ; expansion_mode(fn),
     functor(X1, T, _),
     append(As1, [[T:Args]], As2) ),
   X2 =.. As2.

/* edcg_formula_to_dcg_formula(X1, X2, Args) <-
      */

edcg_formula_to_dcg_formula(X1, X2, V) :-
   X1 = V^X,
   !,
   edcg_formula_to_dcg_formula(X, X2, V).
edcg_formula_to_dcg_formula(X1, X2, V) :-
   X1 = sequence(_,_),
   !,
   add_variable_to_atom(V, X1, X2).
edcg_formula_to_dcg_formula(X1, X2, V) :-
   X1 = {X},
   !,
   X2 = {X, (V = [])}.
edcg_formula_to_dcg_formula(X1, X2, V) :-
   X1 = !,
   !,
   X2 = (!, {V = []}).
edcg_formula_to_dcg_formula(X1, X2, V) :-
   X1 = (_,_),
   !,
   comma_structure_to_list(X1, Xs1),
   !,
   findall([B,C], (
      member(A, Xs1),
      edcg_formula_to_dcg_formula(A, B, C)
   ), Ls),
   maplist(split_tuple, Ls, Xs2, Vs),
   list_to_comma_structure(Xs2, X),
   ( expansion_mode(list),
      X2 = (X, { flatten(Vs, V) })
   ; expansion_mode(fn),
      X2 = (X, {append(Vs, V)})
   ),
   !.
edcg_formula_to_dcg_formula(X1, X2, V) :-
   (X1 = (_;_) ; X1 = (_|_)),
   !,
   semicolon_structure_to_list(X1, Xs1),
   maplist( edcg_formula_to_dcg_formula,
      Xs1, Xs2, Vs),
   maplist( add_variable_binding(V),
      Xs2, Vs, Xsn2),
   list_to_semicolon_structure(Xsn2, X2),
   !.
edcg_formula_to_dcg_formula(X1, X2, V) :-
   X1 = [SingleNonTerminal],
   !,
   X2 = X1,
   V = SingleNonTerminal.
edcg_formula_to_dcg_formula(X1, X2, V) :-
   add_variable_to_atom(V, X1, X2).

edcg_formula_to_dcg_formula_(Vs, Y1, Y2) :-
   edcg_formula_to_dcg_formula(Y1, Y2, Vs).

add_variable_binding(Bind, X2, V, X2n) :-
   X2n = (X2, { Bind = V }).


% meta-call predicates

:- meta_predicate sequence(?, //, ?, ?, ?).

sequence('?', Predicate, [A], Xs, Ys) :-
   apply_sequence(Predicate, [A, Xs, Ys]).
sequence('?', _, [], Xs, Xs).

sequence('+', Predicate, [A|As], Xs, Ys) :-
   apply_sequence(Predicate, [A, Xs, Zs]),
   sequence('*', Predicate, As, Zs, Ys).

sequence('*', _, [], Xs, Xs).
sequence('*', Predicate, [A|As], Xs, Ys) :-
   apply_sequence(Predicate, [A, Xs, Zs]),
   sequence('*', Predicate, As, Zs, Ys).

sequence('**', Predicate, [A|As], Xs, Ys) :-
   apply_sequence(Predicate, [A, Xs, Zs]),
   sequence('**', Predicate, As, Zs, Ys).
sequence('**', _, [], Xs, Xs).

:- meta_predicate apply_sequence(//, ?).

apply_sequence(Module:Predicate, [A, Xs, Ys]) :-
   Predicate =.. [PredicateName|Args],
   append([PredicateName|Args], [A], DCGBodyList),
   DCGBody =.. DCGBodyList,
   phrase(Module:DCGBody, Xs, Ys).

/* add_variable_to_atoms(V, Xs1, Xs2) <-
      */

add_variable_to_atoms(V, [X1|Xs1], [X2|Xs2]) :-
   add_variable_to_atom(V, X1, X2),
   add_variable_to_atoms(V, Xs1, Xs2).
add_variable_to_atoms(_, [], []).


/* add_variable_to_atom(V, X1, X2) <-
      */

add_variable_to_atom(V, X1, X2) :-
   is_list(X1),
   !,
   X2 =.. ['$edcg_append', X1, V].
add_variable_to_atom(V, X1, X2) :-
   X1 =.. As1,
   append(As1, [V], As2),
   X2 =.. As2.


/* '$edcg_append'(Xs, V, Ys, Zs) <-
      */

'$edcg_append'(Xs, V, Ys, Zs) :-
   append(Xs, Zs, Ys),
   Xs = V.

/* comma_structure_to_list(Structure, List) <-
      */

comma_structure_to_list(S, [X|Ys]) :-
   nonvar(S),
   S = (X,Xs),
   !,
   comma_structure_to_list(Xs, Ys).
comma_structure_to_list(X, [X]).


/* list_to_comma_structure(List, Structure) <-
      */

list_to_comma_structure([X|Xs], (X,Ys)) :-
   list_to_comma_structure(Xs, Ys).
list_to_comma_structure([X], X).


/* semicolon_structure_to_list(Structure, List) <-
      */

semicolon_structure_to_list(S, [X|Ys]) :-
   nonvar(S),
   (S = (X;Xs) ; S = (X | Xs)),
   !,
   semicolon_structure_to_list(Xs, Ys).
semicolon_structure_to_list(X, [X]).


/* list_to_semicolon_structure(List, Structure) <-
      */

list_to_semicolon_structure([X|Xs], (X;Ys)) :-
   list_to_semicolon_structure(Xs, Ys).
list_to_semicolon_structure([X], X).

split_tuple([A,B],A,B).
