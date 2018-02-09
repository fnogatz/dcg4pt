# TAP Input/Output Test Suite

Definition of input/output tests for `library(edcgs)` following the [Test Anything Protocol](http://testanything.org/) (TAP).

## Run Tests

The defined tests can be run using the following command:

```shell
swipl -q -g main -t halt -s test.pl
```

This produces a TAP compatible output like the following:

```
TAP version 13
1..4
ok 1 - single [some]
ok 2 - with_argument(1) [some]
ok 3 - verb [eat]
ok 4 - verb_phrase as single verb
```

## Define Tests

Tests are defined in the `test.pl` file after the call of `:- use_module(library(tap))`.
