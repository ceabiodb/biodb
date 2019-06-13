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

# Display values of main variables
$(info "BIODB_CACHE_DIRECTORY=$(BIODB_CACHE_DIRECTORY)")
$(info "BIODB_CACHE_READ_ONLY=$(BIODB_CACHE_READ_ONLY)")
# TODO Set an option for only writing to cache, not reading. This way we can run test and update the cache only.
$(info "DATABASES=$(DATABASES)")
$(info "DONT_TEST_DBS=$(DONT_TEST_DBS)")
$(info "PKG_VERSION=$(PKG_VERSION)")
$(info "GIT_VERSION=$(GIT_VERSION)")

# Default target {{{1
################################################################

all: compile

# Compile {{{1
################################################################

compile:
	R CMD SHLIB src/*.c

# Check and test {{{1
################################################################

check: $(ZIPPED_PKG)
	time R CMD check --no-build-vignettes "$<"
# Use `R CMD check` instead of `devtools::test()` because the later failed once on Travis-CI:
#   Warning in config_val_to_logical(check_incoming) :
#     cannot coerce ‘FALSE false’ to logical
#   Error in if (check_incoming) check_CRAN_incoming(!check_incoming_remote) : 
#     missing value where TRUE/FALSE needed
#   Execution halted

bioc.check: $(ZIPPED_PKG)
	R -q -e 'library(BiocCheck)' # Make sure library is loaded once in order to install the scripts.
	time R CMD BiocCheck --new-package --quit-with-status "$<"

check.version:
#	test "$(PKG_VERSION)" = "$(GIT_VERSION)"
# Does not work anymore

test: check.version
	R -q -e "devtools::test('$(CURDIR)', reporter = c('$(TESTTHAT_REPORTER)', 'fail'))"

win:
	R -q -e "devtools::build_win('$(CURDIR)')"

conda_install_%: clean
	docker build -t biodb.$@ -f tests/dockerfiles/$@.dockerfile .

# Build {{{1
################################################################

$(ZIPPED_PKG) build: doc
	R CMD build .

# Documentation {{{1
################################################################

doc:
	R -q -e "devtools::document('$(CURDIR)')"

vignettes:
	@echo Build vignettes for already installed package, not from local soures.
	R -q -e "devtools::clean_vignettes('$(CURDIR)')"
	R -q -e "devtools::build_vignettes('$(CURDIR)')"

# Deprecated {{{1
################################################################

devtools.check: clean.cache
	R -q -e "results <- devtools::check('$(CURDIR)') ; if (length(results$$errors) > 0 || length(results$$warnings) > 0 || length(results$$notes) > 0) quit(status = 1)"

devtools.build:
	R -q -e "devtools::build('$(CURDIR)')"

# Install {{{1
################################################################

install.deps:
	R -q -e "devtools::install_dev_deps('$(CURDIR)')"

install: uninstall install.local list.classes

install.local:
	R --slave -e "devtools::install_local('$(CURDIR)', dependencies = TRUE)"

list.classes:
	R --slave -e 'library(biodb) ; cat("Exported methods and classes:", paste(" ", ls("package:biodb"), collapse = "\n", sep = ""), sep = "\n")'

uninstall:
	R --slave -e "try(devtools::uninstall('$(CURDIR)'), silent = TRUE)"

# Clean {{{1
################################################################

clean: clean.build
	$(RM) src/*.o src/*.so src/*.dll
	$(RM) -r tests/test.log tests/output tests/test\ *.log

clean.build:
	$(RM) biodb_*.tar.gz

clean.cache:
	$(RM) -r $(BIODB_CACHE_DIRECTORY)

# Phony targets {{{1
################################################################

.PHONY: all clean win test build check vignettes install uninstall devtools.check devtools.build clean.build clean.cache doc check.version
