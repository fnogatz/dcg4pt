:- use_module(library(dcg4pt_expand)).

:- op(1200, xfx, ==>).

sentence ==> noun_phrase, verb_phrase.
noun_phrase ==> determiner, noun.
verb_phrase ==> ( verb ; verb, noun_phrase ).
noun ==> [boy] ; [boys] ; [apple] ; [apples].
determiner ==> [the].
verb ==> [eat] ; [eats].

single ==> [some].

with_argument(1) ==> [some].

sequence_star ==> sequence('*', single).
sequence_question ==> sequence('?', single).

sequences ==> [other], sequence('*', single).

:- op(800, fy, *).
*(A,B,C,D) :- sequence('*', A, B, C, D).

sequence_star_prefix ==> *single.

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
