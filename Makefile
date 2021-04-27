# vi: fdm=marker

# Global variables {{{1
################################################################

# Mute R 3.6 "Registered S3 method overwritten" warning messages.
# Messages that were output:
#     Registered S3 method overwritten by 'R.oo':
#       method        from
#       throw.default R.methodsS3
#     Registered S3 method overwritten by 'openssl':
#       method      from
#       print.bytes Rcpp
export _R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_=no

# Set cache folder
ifndef BIODB_CACHE_DIRECTORY
export BIODB_CACHE_DIRECTORY=$(PWD)/cache
endif

# Set testthat reporter
ifndef TESTTHAT_REPORTER
ifdef VIM
TESTTHAT_REPORTER=summary
else
TESTTHAT_REPORTER=progress
endif
endif

PKG_VERSION=$(shell grep '^Version:' DESCRIPTION | sed 's/^Version: //')
GIT_VERSION=$(shell git describe --tags | sed 's/^v\([0-9.]*\)[a-z]*.*$$/\1/')
ZIPPED_PKG=biodb_$(PKG_VERSION).tar.gz
REF_BIB:=$(wildcard ../public-notes/references.bib)

# R
export _R_CHECK_LENGTH_1_CONDITION_=true
export _R_CHECK_LENGTH_1_LOGIC2_=true
RFLAGS=--slave --no-restore
R_HOME=$(shell /usr/bin/env R $(RFLAGS) RHOME)
R=$(R_HOME)/bin/R

# Set and check name
PKG_NAME := $(notdir $(realpath $(CURDIR)))
ifeq (,$(shell echo $(PKG_NAME) | grep '^biodb\([A-Z][A-Za-z0-9]*\)\?$$'))
$(error "$(PKG_NAME)" is not a standard package name for a biodb extension. The package name for a biodb extension must respect the format "^biodb([A-Z][A-Za-z0-9]*)?")
endif
ifeq (biodb,$(PKG_NAME))
PKG_NAME_CAPS := BIODB
else
PKG_NAME_CAPS := BIODB_$(shell echo $(PKG_NAME) | sed 's/^biodb//' | tr '[:lower:]' '[:upper:]')
endif

# For R CMD SHLIB
export PKG_CXXFLAGS=$(shell $(R) $(RFLAGS) -e "Rcpp:::CxxFlags()")
PKG_CXXFLAGS+=-O
PKG_CXXFLAGS+=-I$(realpath $(shell $(R) $(RFLAGS) -e "cat(file.path(testthat::testthat_examples(),'../include'))"))

# Set test file filter
ifndef TEST_FILE
TEST_FILE=NULL
else
TEST_FILE:='$(TEST_FILE)'
endif

# Check
CHECK_RENVIRON=check.Renviron
export R_CHECK_ENVIRON=$(CHECK_RENVIRON)

# Display values of main variables
$(info "BIODB_CACHE_DIRECTORY=$(BIODB_CACHE_DIRECTORY)")
$(info "BIODB_CACHE_READ_ONLY=$(BIODB_CACHE_READ_ONLY)")
$(info "PKG_VERSION=$(PKG_VERSION)")
$(info "REF_BIB=$(REF_BIB)")
$(info "RFLAGS=$(RFLAGS)")
$(info "PKG_CXXFLAGS=$(PKG_CXXFLAGS)")
$(info "TEST_FILE=$(TEST_FILE)")
$(info "BIODB_TEST_FUNCTIONS=$(BIODB_TEST_FUNCTIONS)")
$(info "PKG_NAME=$(PKG_NAME)")
$(info "PKG_NAME_CAPS=$(PKG_NAME_CAPS)")

# Default target {{{1
################################################################

all: compile

# Compile {{{1
################################################################

compile: R/RcppExports.R
	$(R) $(RFLAGS) CMD SHLIB -o src/biodb.so src/*.cpp

R/RcppExports.R: src/*.cpp
	$(R) $(RFLAGS) -e "Rcpp::compileAttributes('$(CURDIR)')"

# Check and test {{{1
################################################################

# Code coverage
coverage:
	$(R) $(RFLAGS) -e "covr::codecov(token='$(value CODECOV_$(PKG_NAME_CAPS)_TOKEN)', quiet=FALSE)"

check: clean.vignettes $(CHECK_RENVIRON) $(ZIPPED_PKG)
	$(R) $(RFLAGS) CMD check $(ZIPPED_PKG)

bioc.check: clean.vignettes $(CHECK_RENVIRON) $(ZIPPED_PKG)
	$(R) $(RFLAGS) -e 'BiocCheck::BiocCheck("$(ZIPPED_PKG)", `new-package`=TRUE, `quit-with-status`=TRUE, `no-check-formatting`=TRUE)'

bioc.check.clone: clean clean.cache
	$(R) $(RFLAGS) -e 'BiocCheck::BiocCheckGitClone()'

check.all: bioc.check.clone check bioc.check

check.version:
#	test "$(PKG_VERSION)" = "$(GIT_VERSION)"
# Does not work anymore

$(CHECK_RENVIRON):
	wget https://raw.githubusercontent.com/Bioconductor/packagebuilder/master/check.Renviron

test: check.version compile
ifdef VIM
	$(R) $(RFLAGS) -e "devtools::test('$(CURDIR)', filter=$(TEST_FILE), reporter=c('$(TESTTHAT_REPORTER)', 'fail'))" | sed 's!\([^/A-Za-z_-]\)\(test[^/A-Za-z][^/]\+\.R\)!\1tests/testthat/\2!'
else
	$(R) $(RFLAGS) -e "devtools::test('$(CURDIR)', filter=$(TEST_FILE), reporter=c('$(TESTTHAT_REPORTER)', 'fail'))"
endif

long.test: install
	$(R) $(RFLAGS) -e "testthat::test_dir('tests/long')"

test.all: test long.test

win:
	$(R) $(RFLAGS) -e "devtools::check_win_devel('$(CURDIR)')"

conda_install_%: clean
	docker build -t biodb.$@ -f tests/dockerfiles/$@.dockerfile .

# Build {{{1
################################################################

build: $(ZIPPED_PKG)

$(ZIPPED_PKG): doc
	R CMD build .

# Documentation {{{1
################################################################

doc: install.deps R/RcppExports.R
	$(R) $(RFLAGS) -e "devtools::document('$(CURDIR)')"

vignettes: clean.vignettes
	@echo Building vignettes for already installed package, not from local sources.
	$(R) $(RFLAGS) -e "devtools::build_vignettes('$(CURDIR)')"

ifneq ($(REF_BIB),)
vignettes: vignettes/references.bib

vignettes/references.bib: $(REF_BIB)
	cp $< $@
endif

# Deprecated {{{1
################################################################

devtools.check: clean.cache
	$(R) $(RFLAGS) -e "results <- devtools::check('$(CURDIR)') ; if (length(results$$errors) > 0 || length(results$$warnings) > 0 || length(results$$notes) > 0) quit(status = 1)"

devtools.build:
	$(R) $(RFLAGS) -e "devtools::build('$(CURDIR)')"

# Install {{{1
################################################################

install.deps:
	$(R) $(RFLAGS) -e "devtools::install_dev_deps('$(CURDIR)')"

quick.install:
	$(R) $(RFLAGS) -e "devtools::install_local('$(CURDIR)', force=TRUE)"

install: uninstall install.local list.classes

install.local: doc
	$(R) $(RFLAGS) -e "devtools::install_local('$(CURDIR)', dependencies=TRUE, build_manual=TRUE, build_vignettes=TRUE)"

list.classes:
	$(R) $(RFLAGS) -e 'library(biodb) ; cat("Exported methods and classes:", paste(" ", ls("package:biodb"), collapse = "\n", sep = ""), sep = "\n")'

uninstall:
	$(R) $(RFLAGS) -e "try(devtools::uninstall('$(CURDIR)'), silent = TRUE)"

# Clean {{{1
################################################################

clean: clean.build clean.vignettes
	$(RM) src/*.o src/*.so src/*.dll
	$(RM) -r tests/test.log tests/testthat/output tests/testthat/*.log
	$(RM) -r biodb.Rcheck Meta man
	$(RM) .Rhistory R/.Rhistory
	$(RM) -r ..Rcheck
	$(RM) *.log
	$(RM) $(CHECK_RENVIRON)

clean.all: clean clean.cache
	@echo "Clean also what is versioned but can be rebuilt."
	$(RM) inst/extdata/massbank_extract_full.sqlite inst/extdata/chebi_extract.sqlite
	$(RM) man

clean.vignettes:
	$(RM) -r doc

clean.build:
	$(RM) biodb_*.tar.gz

clean.cache:
	$(RM) -r $(BIODB_CACHE_DIRECTORY)

# Phony targets {{{1
################################################################

.PHONY: all clean clean.all win test build check bioc.check bioc.check.clone check.all vignettes install uninstall devtools.check devtools.build clean.build clean.cache doc check.version
