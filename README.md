# Extended DCGs

Extend Definite Clause Grammars (DCG) for Prolog by an additional argument to automatically store the parsing tree.

## Synopsis

```prolog
:- use_module(library(edcgs_expand)).

:- op(1200, xfx, ==>).

% Extended DCGs get expanded to hold additional
%   argument with syntax tree.
sentence ==> noun_phrase(N), verb_phrase(N).
noun_phrase(N) ==> determiner, noun(N).
verb_phrase(N) ==> ( verb(N) ; verb(N), noun_phrase(_) ).
noun(sg) ==> [boy]  ; [apple].
noun(pl) ==> [boys] ; [apples].
determiner ==> [the].
verb(sg) ==> [eats].
verb(pl) ==> [eat].

main :-
  phrase(sentence(Tree), [the, boy, eats, the, apples]),
  print_term(Tree, [indent_arguments(2)]).

% prints:
%   sentence([
%     noun_phrase([ 
%       determiner(the),
%       noun(boy) ]),
%     verb_phrase([
%       verb(eats),
%       noun_phrase([
%         determiner(the),
%         noun(apples) ]) ]) ])
```

## Installation

This pack is available from the [add-on registry of SWI-Prolog](http://www.swi-prolog.org/pack/list).

It can be installed with `pack_install/1`:

```prolog
?- pack_install(edcgs).
```

## Requirements

Only for development purposes the [`tap` pack](http://www.swi-prolog.org/pack/list?p=tap) is needed:

```prolog
?- pack_install(tap).
```

## EDCG Expansion

In most cases you simply want to automatically expand all EDCGs to regular DCGs. To do so, simply call:

```prolog
:- use_module(library(edcgs_expand)).
% and later the definition of EDCGs
```

Additionally you can manually call the predicates to translate an EDCG rule:

```prolog
?- use_module(library(edcgs)).
?- edcg_rule_to_dcg_rule((sentence ==> noun_phrase, verb_phrase), DCG).
DCG = (sentence(X) --> ( ... )).
```

## Usage

You can use the generated predicates like normal DCGs, besides that they provide an additional argument to hold the parse tree. It is automatically added as the very last argument of the DCG body.

`library(edgcs)` has been implemented to provide a tool that generates a parse tree from a given input list but also the other way around, i.e., to generate a list based on a parse tree. So you can also use it this way:

```prolog
?- Tree = noun_phrase([determiner(the), noun(boy)]),
   phrase(noun_phrase(N, Tree), List).
Tree = noun_phrase([determiner(the), noun(boy)]),
N = sg,
List = [the, boy] .
```
