:- op(800, xfx, <=>).

% Define term expansion for TAP tests
term_expansion(DCGBody: PT <=> In, [(Head1 :- Test1), (Head2 :- Test2)]) :-
  format(atom(Head1), '~w > "~s"', [DCGBody, In]),
  format(atom(Head2), '~w < "~s"', [DCGBody, In]),
  DCGBody =.. DCGBody_L,
  % build first test: from input to parsetree
  append(DCGBody_L, [PT1], DCGBody_L1),
  DCGBody1 =.. DCGBody_L1,
  Test1 = (
    phrase(DCGBody1, In, []), !,
    PT1 = PT
  ),
  % build second test: from parsetree to input
  append(DCGBody_L, [PT], DCGBody_L2),
  DCGBody2 =.. DCGBody_L2,
  Test2 = (
    phrase(DCGBody2, In2, []), !,
    In2 = In
  ),
  % register tests
  tap:register_test(Head1),
  tap:register_test(Head2).

% Use term expansion of DCG4PT
:- use_module(library(dcg4pt/expand)).

% DCGs to be tested starting here.
c1 --> "a".
c2 --> "abc".

c(1) --> "a".
c(2) --> "abc".

nonterminal --> c1.
nonterminal(1) --> c1.
nonterminal(2) --> c2.

d_or_e --> "d".
d_or_e --> "e".

or1 --> "d" | "e".

% Example calls to be tested starting here.

:- use_module(library(tap)).

c1: c1("a") <=> `a`.
c2: c2("abc") <=> `abc`.

c(1): c("a") <=> `a`.
c(2): c("abc") <=> `abc`.

d_or_e: d_or_e("d") <=> `d`.
d_or_e: d_or_e("e") <=> `e`.

% or1: or1("d") <=> `d`.

nonterminal: nonterminal(c1("a")) <=> `a`.
nonterminal(1): nonterminal(c1("a")) <=> `a`.
nonterminal(2): nonterminal(c2("abc")) <=> `abc`.

/*
sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, noun.
verb_phrase --> ( verb ; verb, noun_phrase ).
noun --> [boy] ; [boys] ; [apple] ; [apples].
determiner --> [the].
verb --> [eat] ; [eats].

single --> [some].

with_argument(1) --> [some].

sequence_star --> sequence('*', single).
sequence_question --> sequence('?', single).

sequences --> [other], sequence('*', single).

:- op(800, fy, *).
*(A,B,C,D) :- sequence('*', A, B, C, D).

sequence_star_prefix --> *single.

:- use_module(library(tap)).

'single [some]' :-
   phrase(single(X), [some]), !,
   X = single(some).

'with_argument(1) [some]' :-
   phrase(with_argument(1, X), [some]), !,
   X = with_argument(some).

'verb [eat]' :-
   phrase(verb(X), [eat]), !,
   X = verb(eat).

'verb_phrase as single verb' :-
   phrase(verb_phrase(X), [eat]), !,
   X = verb_phrase(verb(_)).

'verb_phrase as conjunction' :-
   phrase(verb_phrase(X), [eat, the, apple]), !,
   X = verb_phrase([verb(_), noun_phrase(_)]).

'sequence_star []' :-
   phrase(sequence_star(X), []), !,
   X = sequence_star([]).

'sequence_star [some]' :-
   phrase(sequence_star(X), [some]), !,
   X = sequence_star([single(some)]).

'sequence_star [some,some]' :-
   phrase(sequence_star(X), [some,some]), !,
   X = sequence_star([single(some), single(some)]).

'sequence_star_prefix [some,some]' :-
   phrase(sequence_star_prefix(X), [some,some]), !,
   X = sequence_star_prefix([single(some), single(some)]).

'sequence_question []' :-
   phrase(sequence_question(X), []), !,
   X = sequence_question([]).

'sequence_question [some]' :-
   phrase(sequence_question(X), [some]), !,
   X = sequence_question([single(some)]).

'sequence_question (not multiples) [some,some]' :-
   \+phrase(sequence_question(_), [some, some]), !.

'sequences [other, some]' :-
   phrase(sequences(X), [other, some]), !,
   X = sequences([other, single(some)]).

'sentence [the, boy, eats, the, apple]' :-
   phrase(sentence(X), [the, boy, eats, the, apple]), !,
   X = sentence([
      noun_phrase([
         determiner(the),
         noun(boy)
      ]),
      verb_phrase([
         verb(eats),
         noun_phrase([
            determiner(the),
            noun(apple)
         ])
      ])
   ]).
*/