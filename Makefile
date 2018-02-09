.PHONY: all test clean

version := $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)

SWIPL := swipl

all: test

version:
	echo $(version)

test:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl
