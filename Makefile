.PHONY: all test clean

version := $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
packfile = dcg4pt-$(version).tgz

SWIPL := swipl

all: test

version:
	echo $(version)

check: test

install:
	echo "(none)"

test:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

package: test
	tar cvzf $(packfile) prolog test pack.pl README.md LICENSE

release: test
	hub release create -m v$(version) v$(version)
