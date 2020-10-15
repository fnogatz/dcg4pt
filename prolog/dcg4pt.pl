:- module(dcg4pt, [
    dcg4pt_rules_to_dcg_rules/0,
    dcg4pt_rule_to_dcg_rule/2,
    '$dcg4pt_append'/4,
    sequence/5,
    call_sequence_ground/6
  ]).

prolog:message(warn(Text)) --> [Text].

dcg4pt_rules_to_dcg_rules :-
  forall( X1 --> Y1,
    ( dcg4pt_rule_to_dcg_rule(X1 --> Y1, X2 --> Y2),
      expand_term(X2 --> Y2, Rule),
      assert(Rule) ) ).

dcg4pt_rule_to_dcg_rule(X1 --> Y1, X2 --> Y2) :-
  X1 =.. [H|Args1],
  Res =.. [H, PT],
  append(Args1, [Res], Args2),
  X2 =.. [H|Args2],
  dcg4pt_formula_to_dcg_formula(Y1, Y2, PT).

dcg4pt_formula_to_dcg_formula(X1, X2, V) :-
  X1 = V^X,
  !,
  dcg4pt_formula_to_dcg_formula(X, X2, V).
dcg4pt_formula_to_dcg_formula(X1, X2, V) :-
  X1 = sequence(_,_),
  !,
  add_variable_to_atom(V, X1, X2).
dcg4pt_formula_to_dcg_formula({ X }, X2 = {X, (V = [])}, V) :-
  !.
dcg4pt_formula_to_dcg_formula(!, (!, {V = []}), V) :-
  !.
dcg4pt_formula_to_dcg_formula(X1, X2, V) :-
  X1 = (_,_),
  !,
  comma_structure_to_list(X1, Xs1),
  !,
  maplist(conj_body, Xs1, Xs2, R0s, R1s),
  R0s = [V|R0s_], % take first
  append(R1s_, [Last], R1s),
  Last = [],
  maplist((=), R0s_, R1s_),
  list_to_comma_structure(Xs2, X2),
  !.
dcg4pt_formula_to_dcg_formula(X1, X2, V) :-
  (X1 = (_;_) ; X1 = (_|_)),
  !,
  semicolon_structure_to_list(X1, Xs1),
  maplist( dcg4pt_formula_to_dcg_formula,
    Xs1, Xs2, Vs),
  maplist( add_variable_binding(V),
    Xs2, Vs, Xsn2),
  list_to_semicolon_structure(Xsn2, X2),
  !.
dcg4pt_formula_to_dcg_formula([SingleTerminal], [SingleTerminal], SingleTerminal) :-
  !.
dcg4pt_formula_to_dcg_formula(X1, X1, X1) :-
  string(X1),
  !.
dcg4pt_formula_to_dcg_formula(X1, X2, V) :-
  add_variable_to_atom(V, X1, X2).
dcg4pt_formula_to_dcg_formula_(Vs, Y1, Y2) :-
  dcg4pt_formula_to_dcg_formula(Y1, Y2, Vs).

add_variable_binding(Bind, X2, V, ({ Bind = V }, X2)).

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
  dcg4pt_formula_to_dcg_formula(A, DCGBody, V),
  % B = ({ append(V, R1, R0) }, DCGBody).
  B = call_sequence_ground(DCGBody, V, R1, R0).
conj_body(A, B, R0, R1) :-
  dcg4pt_formula_to_dcg_formula(A, DCGBody, V),
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
  \+ var(R0),
  !, % parse tree bound
  append(V, R1, R0),
  phrase(DCGBody, In, Out).
call_sequence_ground(DCGBody, V, R1, R0, In, Out) :-
  (\+ var(In) ; attvar(In), get_attr(In, pure_input, _PIO)),
  !, % input bound
  phrase(DCGBody, In, Out),
  append(V, R1, R0).
call_sequence_ground(DCGBody, V, R1, R0, In, Out) :-
  var(R0),
  var(In),
  !, % parse tree and input unbound
  /*
    Normally, this is not intended. Consider the DCG
      symbol  --> ['a'] | ['b'].
      symbols --> sequence('*', symbol).
    With both input and parse tree arguments being
    unbound, this will generate "", "a", "aa", "aaa",
    etc., which most likely will not end in the
    expected result. For instance,
      ?- phrase(symbols(PT), In), In = ['b'].
    will not terminate, as it is first backtracked
    over the sequence, not the symbols.
    Therefore, we show a warning here.
  */
  print_message(warning, warn('Parse tree AND input unbound; this might not work as expected!')),
  phrase(DCGBody, In, Out),
  append(V, R1, R0).

:- meta_predicate sequence(?, //, ?, ?, ?).

sequence('?', DCGBody, [PT]) --> call(DCGBody, PT).
sequence('?', _, []) --> [].

sequence('*', _, []) --> [].
sequence('*', DCGBody, [PT|PTs]) -->
  call(DCGBody, PT),
  sequence('*', DCGBody, PTs).

sequence('**', DCGBody, [PT|PTs]) -->
  call(DCGBody, PT),
  sequence('**', DCGBody, PTs).
sequence('**', _, []) --> [].

sequence('+', DCGBody, [PT|PTs]) -->
  call(DCGBody, PT),
  sequence('*', DCGBody, PTs).

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
  X2 =.. ['$dcg4pt_append', X1, V].
add_variable_to_atom(V, X1, X2) :-
  X1 =.. As1,
  append(As1, [V], As2),
  X2 =.. As2.


/* '$dcg4pt_append'(Xs, V, Ys, Zs) <-
    */

'$dcg4pt_append'(Xs, V, Ys, Zs) :-
  append(Xs, Zs, Ys),
  Xs = V.

comma_structure_to_list(S, [X|Ys]) :-
  nonvar(S),
  S = (X,Xs),
  !,
  comma_structure_to_list(Xs, Ys).
comma_structure_to_list(X, [X]).

list_to_comma_structure([X|Xs], (X,Ys)) :-
  list_to_comma_structure(Xs, Ys).
list_to_comma_structure([X], X).

semicolon_structure_to_list(S, [X|Ys]) :-
  nonvar(S),
  (S = (X;Xs) ; S = (X | Xs)),
  !,
  semicolon_structure_to_list(Xs, Ys).
semicolon_structure_to_list(X, [X]).

list_to_semicolon_structure([X|Xs], (X;Ys)) :-
  list_to_semicolon_structure(Xs, Ys).
list_to_semicolon_structure([X], X).

split_tuple([A,B],A,B).
