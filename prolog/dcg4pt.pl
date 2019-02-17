:- module(dcg4pt, [
      edcg_rules_to_dcg_rules/0,
      edcg_rule_to_dcg_rule/2,
      '$edcg_append'/4,
      sequence/5,
      call_sequence_ground/6
   ]).

edcg_rules_to_dcg_rules :-
   forall( X1-->Y1,
      ( edcg_rule_to_dcg_rule(X1-->Y1, X2-->Y2),
        expand_term(X2-->Y2, Rule),
        assert(Rule) ) ).

edcg_rule_to_dcg_rule(X1-->Y1, X2-->Y2) :-
   edcg_formula_to_dcg_formula(Y1, Y2, Args),
   X1 =.. As1,
   As1 = [H|_],
   Res =.. [H,Args],
   append(As1, [Res], As2),
   X2 =.. As2.

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
   maplist(conj_body, Xs1, Xs2, R0s, R1s),
   R0s = [V|R0s_], % take first
   append(R1s_, [Last], R1s),
   Last = [],
   maplist((=), R0s_, R1s_),
   list_to_comma_structure(Xs2, X),
   X2 = (
      X
   ),
   !.
edcg_formula_to_dcg_formula(X1, X2, V) :-
   (X1 = (_;_) ; X1 = (_|_)),
   !,
   disjunctive_list(X1, Xs1),
   maplist( edcg_formula_to_dcg_formula,
      Xs1, Xs2, Vs),
   maplist( add_variable_binding(V),
      Xs2, Vs, Xsn2),
   disjunctive_list(X2, Xsn2),
   !.
edcg_formula_to_dcg_formula(X1, X2, V) :-
   X1 = [SingleTerminal],
   !,
   X2 = X1,
   V = SingleTerminal.
edcg_formula_to_dcg_formula(X1, X2, V) :-
   string(X1), !,
   X2 = X1,
   V = X1.
edcg_formula_to_dcg_formula(X1, X2, V) :-
   add_variable_to_atom(V, X1, X2).

edcg_formula_to_dcg_formula_(Vs, Y1, Y2) :-
   edcg_formula_to_dcg_formula(Y1, Y2, Vs).

add_variable_binding(Bind, X2, V, X2n) :-
   X2n = ({ Bind = V }, X2).

conj_body(A, B, R0, R1) :-
   A = *(C),
   !,
   conj_body(sequence('*', C), B, R0, R1).
conj_body(A, B, R0, R1) :-
   A = ?(C),
   !,
   conj_body(sequence('?', C), B, R0, R1).
conj_body(A, B, R0, R1) :-
   A = sequence(_, _),
   !,
   edcg_formula_to_dcg_formula(A, DCGBody, V),
   % B = ({ append(V, R1, R0) }, DCGBody).
   B = call_sequence_ground(DCGBody, V, R1, R0).
conj_body(A, B, R0, R1) :-
   edcg_formula_to_dcg_formula(A, DCGBody, V),
   B = (
      { R0 = [V|R1] },
      DCGBody
   ).

% meta-call predicates

/*
   call_sequence_ground(DCGBody, V, Tree_List_Rest, In, Out) <-

   V is the last argument of DCGBody, so it's the generated
   parsing tree. Originally, we want to simply call
      phrase(DCGBody, In, Out)
   and put its result V in front of the remaining list R1
   to get R0, i.e.:
      Translated_Body = (DCGBody, { append(V, R1, R0) })
   However, there are two possibilities, depending on whether
   phrase(some(?Tree),?In,?Out) is called with the `In`
   bound or `Tree`. In the first case we want to generate
   the appropriate parsing tree; in the latter case the
   input list for a corresponding parsing tree should be
   generated. That's why we need two different translated
   rule bodies: either by calling the DCGBody at first;
   or by splitting the list of parsing tree elements at
   first. The latter case is equivalent to:
      Translated_Body = ({ append(V, R1, R0) }, DCGBody)
   The meta-predicate call_sequence_ground/6 applies this
   distinction and calls the translated body in the right
   order.
*/
:- meta_predicate call_sequence_ground(//, ?, ?, ?, ?, ?).
call_sequence_ground(DCGBody, V, R1, R0, In, Out) :-
   \+var(R0),
   !,
   append(V, R1, R0),
   phrase(DCGBody, In, Out).
call_sequence_ground(DCGBody, V, R1, R0, In, Out) :-
   \+var(In),
   !,
   phrase(DCGBody, In, Out),
   append(V, R1, R0).

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


disjunctive_list(Term, [Term]) :-
  Term \= (_;_),
  Term \= (_|_),
  !.
disjunctive_list(Term, [T|Ts]) :-
  var(Term), !,
  disjunctive_list(TermR, Ts),
  Term = (T;TermR).
disjunctive_list(Term, [T|Ts]) :-
  nonvar(Term),
  ( Term = (T;TermR) ; Term = (T|TermR) ), !,
  disjunctive_list(TermR, Ts).

split_tuple([A,B],A,B).
