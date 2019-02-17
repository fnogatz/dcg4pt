:- op(800, xfx, <=>). % succeeding tests
:- op(800, xfx, <#>). % failing tests

heads(Symbol, DCGBody, In, Head1, Head2) :-
  In = [First|_], integer(First), !,
  format(atom(Head1), '~w ~w> "~s"', [DCGBody, Symbol, In]),
  format(atom(Head2), '~w ~w< "~s"', [DCGBody, Symbol, In]).
heads(Symbol, DCGBody, In, Head1, Head2) :-
  format(atom(Head1), '~w ~w> ~w', [DCGBody, Symbol, In]),
  format(atom(Head2), '~w ~w< ~w', [DCGBody, Symbol, In]).

% Define term expansion for TAP tests

%% succeeding tests, using `<=>`
term_expansion(DCGBody: PT <=> In, [(Head1 :- Test1), (Head2 :- Test2)]) :-
  heads('', DCGBody, In, Head1, Head2),
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

%% failing tests, using `<#>`
term_expansion(DCGBody: PT <#> In, [(Head1 :- Test1), (Head2 :- Test2)]) :-
  heads('!', DCGBody, In, Head1, Head2),
  DCGBody =.. DCGBody_L,
  % build first test: from input to parsetree
  append(DCGBody_L, [_PT1], DCGBody_L1),
  DCGBody1 =.. DCGBody_L1,
  Test1 = (
    \+ phrase(DCGBody1, In, [])
  ),
  % build second test: from parsetree to input
  append(DCGBody_L, [PT], DCGBody_L2),
  DCGBody2 =.. DCGBody_L2,
  Test2 = (
    \+ phrase(DCGBody2, _In2, [])
  ),
  % register tests
  tap:register_test(Head1),
  tap:register_test(Head2).

% Use term expansion of DCG4PT
:- use_module(library(dcg4pt/expand)).

% DCGs to be tested starting here.

%% Test Section I: DCGs

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

%% Test Section II: DCGs

sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, noun.
verb_phrase --> ( verb ; verb, noun_phrase ).
noun --> [boy] ; [boys] ; [apple] ; [apples].
determiner --> [the].
verb --> [eat] ; [eats].

single --> [some].

with_argument(1) --> [some].

sequence_star --> sequence('*', single).
sequence_plus --> sequence('+', single).
sequence_question --> sequence('?', single).

sequences --> [other], sequence('*', single).

:- op(800, fy, *).
*(A,B,C,D) :- sequence('*', A, B, C, D).

sequence_star_prefix --> *single.

% Example calls to be tested starting here.

:- use_module(library(tap)).

%% Test Section I: Tests

c1: c1("a") <=> `a`.
c2: c2("abc") <=> `abc`.

c(1): c("a") <=> `a`.
c(2): c("abc") <=> `abc`.

d_or_e: d_or_e("d") <=> `d`.
d_or_e: d_or_e("e") <=> `e`.

or1: or1("d") <=> `d`.
or1: or1("e") <=> `e`.

nonterminal: nonterminal(c1("a")) <=> `a`.
nonterminal(1): nonterminal(c1("a")) <=> `a`.
nonterminal(2): nonterminal(c2("abc")) <=> `abc`.

% Test Section II: Tests

single: single(some) <=> [some].
with_argument(1): with_argument(some) <=> [some].
verb: verb(eat) <=> [eat].
verb_phrase: verb_phrase(verb(eat)) <=> [eat].
sequence_star: sequence_star([]) <=> [].
sequence_star: sequence_star([single(some)]) <=> [some].
sequence_star: sequence_star([single(some), single(some)]) <=> [some,some].
sequence_plus: sequence_plus([single(some)]) <=> [some].
sequence_plus: sequence_plus([single(some), single(some)]) <=> [some,some].
sequence_star_prefix: sequence_star_prefix([single(some), single(some)]) <=> [some,some].
sequence_question: sequence_question([]) <=> [].
sequence_question: sequence_question([single(some)]) <=> [some].
sequences: sequences([other, single(some)]) <=> [other,some].
sentence:
  sentence([
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
  ]) <=> [the, boy, eats, the, apple].

sequence_plus: sequence_plus([]) <#> [].
sequence_question:
  sequence_question([single(some), single(some)]) <#> [some,some].
