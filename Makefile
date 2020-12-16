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
$(info "REF_BIB=$(REF_BIB)")

# Display values of main variables
$(info "BIODB_CACHE_DIRECTORY=$(BIODB_CACHE_DIRECTORY)")
$(info "BIODB_CACHE_READ_ONLY=$(BIODB_CACHE_READ_ONLY)")
$(info "PKG_VERSION=$(PKG_VERSION)")

RFLAGS=--slave --no-restore

# Set test file filter
ifndef TEST_FILE
TEST_FILE=NULL
else
TEST_FILE:='$(TEST_FILE)'
endif

# Default target {{{1
################################################################

all: compile

# Compile {{{1
################################################################

compile:
	R CMD SHLIB src/*.c

# Check and test {{{1
################################################################

check: clean.vignettes $(ZIPPED_PKG)
	time R CMD check --no-build-vignettes "$(ZIPPED_PKG)"
# Use `R CMD check` instead of `devtools::test()` because the later failed once on Travis-CI:
#   Warning in config_val_to_logical(check_incoming) :
#     cannot coerce ‘FALSE false’ to logical
#   Error in if (check_incoming) check_CRAN_incoming(!check_incoming_remote) : 
#     missing value where TRUE/FALSE needed
#   Execution halted

full.check: clean.vignettes $(ZIPPED_PKG)
	time R CMD check "$(ZIPPED_PKG)"

bioc.check: PATH:=$(PATH):$(shell R $(RFLAGS) -e 'cat(pkgload::inst("BiocCheck"), "script", sep="/")')
bioc.check: clean.vignettes $(ZIPPED_PKG)
	#PATH=$$PATH:$$(R $(RFLAGS) -e 'cat(pkgload::inst("BiocCheck"), "script", sep="/")') time R CMD BiocCheck --new-package --quit-with-status --no-check-formatting "$(ZIPPED_PKG)"
	time R CMD BiocCheck --new-package --quit-with-status --no-check-formatting "$(ZIPPED_PKG)"

check.version:
#	test "$(PKG_VERSION)" = "$(GIT_VERSION)"
# Does not work anymore

test: check.version
	R $(RFLAGS) -e "devtools::test('$(CURDIR)', filter=$(TEST_FILE), reporter=c('$(TESTTHAT_REPORTER)', 'fail'))"

win:
	R $(RFLAGS) -e "devtools::check_win_devel('$(CURDIR)')"

conda_install_%: clean
	docker build -t biodb.$@ -f tests/dockerfiles/$@.dockerfile .

# Build {{{1
################################################################

$(ZIPPED_PKG) build: doc
	R CMD build .

# Documentation {{{1
################################################################

doc: install.deps
	R $(RFLAGS) -e "devtools::document('$(CURDIR)')"

vignettes: clean.vignettes
	@echo Building vignettes for already installed package, not from local sources.
	time R $(RFLAGS) -e "devtools::build_vignettes('$(CURDIR)')"

ifneq ($(REF_BIB),)
vignettes: vignettes/references.bib

vignettes/references.bib: $(REF_BIB)
	cp $< $@
endif

# Deprecated {{{1
################################################################

devtools.check: clean.cache
	R $(RFLAGS) -e "results <- devtools::check('$(CURDIR)') ; if (length(results$$errors) > 0 || length(results$$warnings) > 0 || length(results$$notes) > 0) quit(status = 1)"

devtools.build:
	R $(RFLAGS) -e "devtools::build('$(CURDIR)')"

# Install {{{1
################################################################

install.deps:
	R $(RFLAGS) -e "devtools::install_dev_deps('$(CURDIR)')"

quick.install:
	R $(RFLAGS) -e "devtools::install_local('$(CURDIR)', force=TRUE)"

install: uninstall install.local list.classes

install.local: doc
	R $(RFLAGS) -e "devtools::install_local('$(CURDIR)', dependencies=TRUE, build_manual=TRUE, build_vignettes=TRUE)"

list.classes:
	R $(RFLAGS) -e 'library(biodb) ; cat("Exported methods and classes:", paste(" ", ls("package:biodb"), collapse = "\n", sep = ""), sep = "\n")'

uninstall:
	R $(RFLAGS) -e "try(devtools::uninstall('$(CURDIR)'), silent = TRUE)"

# Clean {{{1
################################################################

clean: clean.build clean.vignettes
	$(RM) src/*.o src/*.so src/*.dll
	$(RM) -r tests/test.log tests/output tests/test\ *.log
	$(RM) -r biodb.Rcheck Meta man

clean.vignettes:
	$(RM) -r doc

clean.build:
	$(RM) biodb_*.tar.gz

clean.cache:
	$(RM) -r $(BIODB_CACHE_DIRECTORY)

# Phony targets {{{1
################################################################

.PHONY: all clean win test build check vignettes install uninstall devtools.check devtools.build clean.build clean.cache doc check.version
