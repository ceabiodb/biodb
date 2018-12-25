# vi: fdm=marker

# Global variables {{{1
################################################################

export BIODB_CACHE_DIRECTORY=$(HOME)/.biodb.dev.check.cache
PKG_VERSION=$(shell grep '^Version:' DESCRIPTION | sed 's/^Version: //')
GIT_VERSION=$(shell git describe --tags | sed 's/^v\([0-9.]*\)[a-z]*.*$$/\1/')
ZIPPED_PKG=biodb_$(PKG_VERSION).tar.gz

# Default target {{{1
################################################################

all:

# Check and test {{{1
################################################################

check: $(ZIPPED_PKG) clean.cache
	R CMD check --as-cran "$<"
	# Use `R CMD check` instead of `devtools::test()` because the later failed once Travis-CI:
	#   Warning in config_val_to_logical(check_incoming) :
	#     cannot coerce ‘FALSE false’ to logical
	#   Error in if (check_incoming) check_CRAN_incoming(!check_incoming_remote) : 
	#     missing value where TRUE/FALSE needed
	#   Execution halted

check.version:
	test "$(PKG_VERSION)" = "$(GIT_VERSION)"

test: check.version
	R -q -e "devtools::test('$(CURDIR)', reporter = c('progress', 'fail'))"

win:
	R -q -e "devtools::build_win('$(CURDIR)')"

conda_install_%: clean
	docker build -t biodb.$@ -f tests/dockerfiles/$@.dockerfile .

# Build {{{1
################################################################

build: $(ZIPPED_PKG)

$(ZIPPED_PKG): doc
	cd "$(dir $(CURDIR))" && R CMD build "$(notdir $(CURDIR))" && mv "$@" "$(CURDIR)"

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
	R --slave -e 'library(biodb) ; cat("Exported methods and classes:", paste(" ", ls("package:biodb"), collapse = "\\n", sep = ""), sep = "\\n")'

uninstall:
	R --slave -e "try(devtools::uninstall('$(CURDIR)'), silent = TRUE)"

# Clean {{{1
################################################################

clean: clean.cache clean.build
	$(RM) src/*.o src/*.so src/*.dll
	$(RM) -r tests/test.log tests/output tests/test\ *.log

clean.build:
	$(RM) biodb_*.tar.gz

clean.cache:
	$(RM) -r $(BIODB_CACHE_DIRECTORY)

# Phony targets {{{1
################################################################

.PHONY: all clean win test check vignettes install uninstall devtools.check devtools.build clean.build clean.cache doc check.version
