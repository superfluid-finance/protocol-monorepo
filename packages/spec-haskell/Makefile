CABAL ?= cabal
DEV_TARGETS ?= test
TEST_SHOW_DETAILS_MODE ?= direct # alternatively: always | failure | never
TEST_PROP_NUM_RUNS ?= 1000
TEST_BUILDDIR ?= dist-test
TEST_COVERAGE_BUILDDIR ?= dist-coverage

CABAL_TEST = $(CABAL) --builddir=$(TEST_BUILDDIR)
TEST_OPTIONS = \
	--test-show-details=$(TEST_SHOW_DETAILS_MODE) \
	--test-options="--maximum-generated-tests=$(TEST_PROP_NUM_RUNS)"

default: build

prepare:
	$(CABAL_TEST) v2-update

build:
	$(CABAL_TEST) v2-build all

clean: clean-dist

clean-dist:
	rm -rf dist-*

.PHONY: default prepare build clean clean-*

demo-expo: build
	$(CABAL_TEST) v2-exec superfluid-validator -- --demo expo

demo-dfa: build
	$(CABAL_TEST) v2-exec superfluid-validator -- --demo dfa > dfa.ignore.dat
	gnuplot -p -e " \
		plot 'dfa.ignore.dat' using 1:2 with lines title 'alice', \
			 '' using 1:3 with lines title 'bob', \
			 '' using 1:4 with lines title 'carol', \
			 '' using 1:5 with lines title 'dan' \
		"
	rm -f dfa.ignore.dat

test:
	$(CABAL_TEST) \
		v2-test --enable-tests \
		$(TEST_OPTIONS) \
		all

test-coverage:
	$(CABAL) --builddir=$(TEST_COVERAGE_BUILDDIR) --project-file=cabal.project.coverage \
		v2-test --enable-tests --enable-coverage \
		$(TEST_OPTIONS) \
		all

dev:
	nodemon -e hs,cabal -i dist -x "make $(DEV_TARGETS) || exit 1"

repl-simple:
	$(CABAL_TEST) repl superfluid-protocol-system-simple

.PHONY: demo-* test test-* dev repl-*

cloc:
	@echo '**** Core Source Files ****'
	cloc packages/core/src
	@echo

	@echo '**** Simple System Source Files ****'
	cloc packages/simple/src
	@echo

	@echo '**** Validator Source Files ****'
	cloc packages/validator/src
	@echo

	@echo '**** Test Files ****'
	cloc packages/*/test
	@echo

.PHONY: cloc
