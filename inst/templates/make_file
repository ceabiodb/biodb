# Makefile for biodb extensions packages, version 1.4.14
# vi: ft=make

# Mute R 3.6 "Registered S3 method overwritten" warning messages.
# Messages that were output:
#     Registered S3 method overwritten by 'R.oo':
#       method        from
#       throw.default R.methodsS3
#     Registered S3 method overwritten by 'openssl':
#       method      from
#       print.bytes Rcpp
export _R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_=no

# Constants
COMMA := ,

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

# Get versions
PKG_VERSION=$(shell grep '^Version:' DESCRIPTION | sed 's/^Version: //')
PKG_MAJOR=$(shell echo $(PKG_VERSION) | sed "s/\..*$$//")
ifeq (0,$(PKG_MAJOR))
	NEW_PKG=true
endif
ifeq (,$(R_VERSION))
#R_VERSION=$(shell grep '^ *R ' DESCRIPTION | sed 's/^.*[ =]\([0-9]\.[0-9]\(\.[0-9]\)\?\).*$$/\1/')
R_VERSION=4.2.1
endif

# Use system tar instead of R built-in tar in order to avoid the following warning:
# Warning: invalid uid value replaced by that for user 'nobody'
export R_BUILD_TAR=tar

# Bioconductor check flags
BIOC_CHECK_FLAGS=quit-with-status
ifeq (true,$(NEWPKG))
BIOC_CHECK_FLAGS+=new-package
endif
CHECK_RENVIRON=check.Renviron
export R_CHECK_ENVIRON=$(shell realpath $(CHECK_RENVIRON))
RENVIRON_BIOC=Renviron.bioc
export R_ENVIRON_USER=$(shell realpath $(RENVIRON_BIOC))
RENVIRON_FILES=$(CHECK_RENVIRON) $(RENVIRON_BIOC)

# Check files
ifeq (,$(wildcard DESCRIPTION))
$(error Missing file DESCRIPTION)
endif

# Set cache folder
CACHE=$(CURDIR)/cache
LONG_CACHE=$(CURDIR)/cache.long
export BIODB_CACHE_DIRECTORY=$(CACHE)

# Set testthat reporter
ifndef TESTTHAT_REPORTER
ifdef VIM
TESTTHAT_REPORTER=summary

# Call stack
# 8. .self$.parseDbLinks(parsed.content) R/KeggCompoundEntry.R:37:4
ERROR_MSG_FILTER= | sed -e 's!^ *\([0-9]\+\. \)!Callstack \1!'

# Adjust path to test source file in tests/testthat
ERROR_MSG_FILTER+= -e 's!\([^/A-Za-z_-]\)\(test[^/A-Za-z][^/]\+\.R\)!\1tests/testthat/\2!'

# TODO Adjust path to test source file in longtests/testthat

else
TESTTHAT_REPORTER=progress
ERROR_MSG_FILTER=
endif
endif

# Enable compiling
ifneq (,$(wildcard src))
COMPILE=compile
endif

# Set zip filename
ZIPPED_PKG=$(PKG_NAME)_$(PKG_VERSION).tar.gz

# Configure R version to use
export _R_CHECK_LENGTH_1_CONDITION_=true
export _R_CHECK_LENGTH_1_LOGIC2_=true
RFLAGS:=--slave --no-restore
R_FRONT:=$(wildcard $(CURDIR)/R_front $(CURDIR)/inst/templates/R_front)
#ifeq (,$(R_FRONT))
#export R_HOME=$(shell /usr/bin/env R $(RFLAGS) RHOME)
#R=R
#else
export R_HOME:=$(shell bash $(R_FRONT) -n --r-version $(R_VERSION) --print-home)
R:=$(shell bash $(R_FRONT) -n --r-version $(R_VERSION) --print-bin)
#endif

# For R CMD SHLIB
ifdef COMPILE
	export PKG_CXXFLAGS:=$(shell $(R) $(RFLAGS) -e "install.packages(Filter(function(pkg) ! require(pkg, character.only=TRUE), c('Rcpp', 'devtools', 'testthat')), dependencies=TRUE, repos='https://cloud.r-project.org/') ; Rcpp:::CxxFlags()" 2>/dev/null | tail -n 1)
	PKG_CXXFLAGS+=-I$(realpath $(shell $(R) $(RFLAGS) -e "cat(file.path(testthat::testthat_examples(),'../include'))"))
endif

# Set test file filter
ifndef TEST_FILE
TEST_FILE=NULL
else
TEST_FILE:='$(TEST_FILE)'
endif


# Default target
all:

# Debug: display values of main variables
debug::
	$(info BIODB_CACHE_DIRECTORY=$(BIODB_CACHE_DIRECTORY))
	$(info BIODB_TEST_FUNCTIONS=$(BIODB_TEST_FUNCTIONS))
	$(info CODECOV_$(PKG_NAME_CAPS)_TOKEN=$(value CODECOV_$(PKG_NAME_CAPS)_TOKEN))
	$(info PKG_CXXFLAGS=$(PKG_CXXFLAGS))
	$(info PKG_NAME=$(PKG_NAME))
	$(info PKG_NAME_CAPS=$(PKG_NAME_CAPS))
	$(info PKG_VERSION=$(PKG_VERSION))
	$(info R=$(R))
	$(info RFLAGS=$(RFLAGS))
	$(info R_CHECK_ENVIRON=$(R_CHECK_ENVIRON))
	$(info R_ENVIRON_USER=$(R_ENVIRON_USER))
	$(info R_FRONT=$(R_FRONT))
	$(info R_HOME=$(R_HOME))
	$(info R_VERSION=$(R_VERSION))
	$(info TEST_FILE=$(TEST_FILE))

# Rebuild all & install
rebuild:
	# We use a recursive call to force the clean target to be called each time needed (like for checking)
	$(MAKE) clean.all
	$(MAKE) compile
	$(MAKE) doc
	$(MAKE) test.all
	$(MAKE) check.all
	$(MAKE) install

compile: $(COMPILE)

# Compiling
ifdef COMPILE
compile: R/RcppExports.R src/RcppExports.cpp
	$(R) $(RFLAGS) CMD SHLIB -o src/$(PKG_NAME).so src/*.cpp

R/RcppExports.R src/RcppExports.cpp: src/*.cpp
	$(R) $(RFLAGS) -e "Rcpp::compileAttributes('$(CURDIR)')"
endif

# Code coverage
coverage:
	$(R) $(RFLAGS) -e "covr::codecov(token='$(value CODECOV_$(PKG_NAME_CAPS)_TOKEN)', quiet=FALSE)"

# Plain check
check: clean.vignettes $(RENVIRON_FILES) $(ZIPPED_PKG)
	$(R) $(RFLAGS) CMD check $(ZIPPED_PKG)

# Bioconductor check
bioc.check: clean.vignettes $(RENVIRON_FILES) $(ZIPPED_PKG)
	$(R) $(RFLAGS) -e 'if ( ! require(BiocManager)) install.packages("BiocManager", dependencies=TRUE, repos="https://cloud.r-project.org/") ; if ( ! require(BiocCheck)) BiocManager::install("BiocCheck") ; BiocCheck::BiocCheck("$(ZIPPED_PKG)"$(patsubst %,$(COMMA) `%`=TRUE,$(BIOC_CHECK_FLAGS)))' 2>&1 | sed 's!^ *\(R\|man\|vignettes\)/!Issue in \1/!'

# Bioconductor check Git clone
bioc.check.clone: clean clean.cache
	$(R) $(RFLAGS) -e 'if ( ! require(BiocManager)) install.packages("BiocManager", dependencies=TRUE, repos="https://cloud.r-project.org/") ; if ( ! require(BiocCheck)) BiocManager::install("BiocCheck") ; BiocCheck::BiocCheckGitClone()'

check.all: bioc.check.clone check bioc.check

$(CHECK_RENVIRON):
	wget -O $@ 'https://raw.githubusercontent.com/Bioconductor/packagebuilder/master/check.Renviron'

$(RENVIRON_BIOC):
	wget -O $@ 'http://bioconductor.org/checkResults/devel/bioc-LATEST/Renviron.bioc'

longtests: BIODB_CACHE_DIRECTORY=$(LONG_CACHE)
tests longtests: $(COMPILE)
	$(R) $(RFLAGS) -e "testthat::test_dir('$(CURDIR)/$@/testthat', filter=$(TEST_FILE), reporter=c('$(TESTTHAT_REPORTER)', 'fail'), package='$(PKG_NAME)', load_package='source')" $(ERROR_MSG_FILTER)
	
# Run testthat tests
test: tests

# Run testthat long tests
test.long: longtests

# Run all tests
test.all: tests longtests

# Launch Windows tests on server
win:
	$(R) $(RFLAGS) -e "devtools::check_win_devel('$(CURDIR)')"

# Build package zip
pkg: $(ZIPPED_PKG)

# Make zip
$(ZIPPED_PKG): clean.vignettes doc
	$(R) $(RFLAGS) CMD build .

NAMESPACE: # First time creation, then roxygen2 will fill it and keep it updated
	echo "# Generated by roxygen2: do not edit by hand" >$@

# Generation documentation
ifdef COMPILE
doc: install.deps NAMESPACE R/RcppExports.R
else
doc: install.deps NAMESPACE
endif
	$(R) $(RFLAGS) -e "devtools::document('$(CURDIR)')"

# Generate vignettes
vignettes: clean.vignettes
	@command -v pandoc >/dev/null || { echo "pandoc tool is needed."; false; }
	$(R) $(RFLAGS) -e "devtools::build_vignettes('$(CURDIR)')"

upgrade:
	$(R) $(RFLAGS) -e 'biodb::ExtPackage$$new(".")$$upgrade()'

# Install dependencies
install.deps:
	$(R) $(RFLAGS) -e "devtools::install_dev_deps('$(CURDIR)')"

# Install package
install: uninstall
	$(R) $(RFLAGS) -e "devtools::install_local('$(CURDIR)', dependencies=TRUE)"

# Uninstall package
uninstall:
	$(R) $(RFLAGS) -e "try(devtools::uninstall('$(CURDIR)'), silent=TRUE)"

# Clean all, included biodb cache
clean.all: clean clean.cache

# Clean
clean: clean.vignettes
ifdef COMPILE
	$(RM) src/*.o src/*.so src/*.dll
endif
	$(RM) -r *tests/*/output
	$(RM) -r $(PKG_NAME).Rcheck
	$(RM) -r $(PKG_NAME).BiocCheck
	$(RM) -r Meta
	$(RM) *.log
	$(RM) .Rhistory
	$(RM) $(PKG_NAME)_*.tar.gz
	$(RM) $(RENVIRON_FILES)

clean.all: clean clean.cache
	@echo "Clean also what is versioned but can be rebuilt."
	$(RM) -r inst/extdata/generated
	$(RM) -r man

# Clean vignettes
clean.vignettes:
	$(RM) vignettes/*.R vignettes/*.html
	$(RM) -r doc

# Clean biodb cache
clean.cache:
	$(RM) -r $(CACHE)
	$(RM) -r $(LONG_CACHE)

.PHONY: vignettes doc tests longtests
