.PHONY: all test clean

SWIPL ?= swipl

version = $(shell $(SWIPL) -q -s pack -g 'version(V),writeln(V)' -t halt)
packfile = dcg4pt-$(version).tgz
pwd := $(shell pwd)

all: install

version:
	@echo $(version)

check: test

install: install.packs

install.packs: install.packs.tap

install.packs.tap:
	@$(SWIPL) -g 'pack_install(tap,[interactive(false)]),halt(0)' -t 'halt(1)'

test:
	$(SWIPL) -q -p library=$(pwd)/prolog -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

package: test
	tar cvzf $(packfile) prolog test pack.pl README.md LICENSE Makefile

release: test
	hub release create -m v$(version) v$(version)
