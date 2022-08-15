#
# Configurations
#
CABAL ?= cabal
DEV_TARGETS ?= test
TEST_SHOW_DETAILS_MODE ?= direct # alternatively: always | failure | never
TEST_PROP_NUM_RUNS ?= 1000
TEST_BUILDDIR ?= dist-test
TEST_COVERAGE_BUILDDIR ?= dist-coverage
DOCS_BUILDDIR ?= dist-docs

CABAL_TEST     = $(CABAL) --builddir=$(TEST_BUILDDIR)
CABAL_COVERAGE = $(CABAL) --builddir=$(TEST_COVERAGE_BUILDDIR)
CABAL_DOCS     = $(CABAL) --builddir=$(DOCS_BUILDDIR)

TEST_OPTIONS = \
	--test-show-details=$(TEST_SHOW_DETAILS_MODE) \
	--test-options="--maximum-generated-tests=$(TEST_PROP_NUM_RUNS)"

default: build

#
# Build
#
build:
	$(CABAL) v2-build all

build-test:
	$(CABAL_TEST) v2-build all

clean:
	rm -rf $(DOCS_BUILDDIR)
	rm -rf $(TEST_BUILDDIR)
	rm -rf $(TEST_COVERAGE_BUILDDIR)
	rm -rf dist-*

.PHONY: default build* clean

#
# Testing
#
test:
	$(CABAL_TEST) v2-test all --enable-tests \
		$(TEST_OPTIONS)

test-coverage:
	$(CABAL_COVERAGE) v2-build all --enable-tests --enable-coverage
	$(CABAL_COVERAGE) v2-test all --enable-tests --enable-coverage \
		$(TEST_OPTIONS)

.PHONY: test test-*

#
# Demos
#
demo-expo: build-test
	$(CABAL_TEST) v2-exec superfluid-validator -- --demo expo

demo-dfa: build-test
	$(CABAL_TEST) v2-exec superfluid-validator -- --demo dfa | ./utils/self-gnuplot.sh

demo-cfda: build-test
	$(CABAL_TEST) v2-exec superfluid-validator -- --demo cfda | ./utils/self-gnuplot.sh

.PHONY: demo-*

#
# Docs
#
docs-haddock:
	$(CABAL_DOCS) haddock all

YELLOWPAPER_ROOT = $(DOCS_BUILDDIR)/yellowpaper

$(YELLOWPAPER_ROOT)/tex:
	mkdir -p "$@"
$(YELLOWPAPER_ROOT)/tex/MoneyDistributionConcepts.tex: packages/core/src/Money/Distribution/Concepts.lhs
	./utils/lhs2tex.sh "$<" > "$@"
$(YELLOWPAPER_ROOT)/tex/Communism.tex: packages/core/src/Money/Distribution/Communism.lhs
	./utils/lhs2tex.sh "$<" > "$@"
$(YELLOWPAPER_ROOT)/tex/lhsfmt.tex:
	echo '%include lhs2TeX.fmt' | lhs2TeX > "$@"

.SECONDEXPANSION:
$(YELLOWPAPER_ROOT)/tex/%: yellowpaper/$$(notdir $$*)
	cp "$<" "$@"

YELLOWPAPER_INPUTS = $(YELLOWPAPER_ROOT)/tex \
	$(YELLOWPAPER_ROOT)/tex/MoneyDistributionConcepts.tex \
	$(YELLOWPAPER_ROOT)/tex/Communism.tex \
	$(YELLOWPAPER_ROOT)/tex/lhsfmt.tex \
	$(YELLOWPAPER_ROOT)/tex/lhsfmt.tex \
	$(YELLOWPAPER_ROOT)/tex/Biblio.bib \
	$(YELLOWPAPER_ROOT)/tex/Paper.tex
$(YELLOWPAPER_ROOT)/pdflatex/superfluid-yellowpaper.pdf: $(YELLOWPAPER_INPUTS)
	ls $(YELLOWPAPER_ROOT)/tex
	rm -rf $(YELLOWPAPER_ROOT)/pdflatex
	mkdir -p $(YELLOWPAPER_ROOT)/pdflatex
	cd $(YELLOWPAPER_ROOT)/pdflatex; { \
		pdflatex -shell-escape -jobname=superfluid-yellowpaper ../tex/Paper.tex; \
		biber superfluid-yellowpaper.bcf; \
		pdflatex -shell-escape -jobname=superfluid-yellowpaper ../tex/Paper.tex; \
	}

docs-yellowpaper: $(YELLOWPAPER_ROOT)/pdflatex/superfluid-yellowpaper.pdf

docs: docs-haddock docs-yellowpaper

.PHONY: docs-haddock docs

#
# Dev Environment
#
dev:
	nodemon \
		-e lhs,hs,cabal \
		-i dist-newstyle/ -i "$(TEST_BUILDDIR)/" -i "$(TEST_COVERAGE_BUILDDIR)/" \
		-x "make $(DEV_TARGETS) || exit 1"

freeze:
	rm -f cabal.project.freeze
	$(CABAL) v2-freeze

hlint:
	hlint packages

repl-simple:
	$(CABAL) repl superfluid-protocol-system-simple

cloc:
	@echo '**** Core Source Files ****'
	cloc packages/core/src*
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

.PHONY: dev freeze hlint repl-* cloc
