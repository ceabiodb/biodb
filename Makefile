export BIODB_CACHE_DIRECTORY=$(HOME)/.biodb.dev.check.cache
PKG_VERSION=$(shell grep '^Version:' DESCRIPTION | sed 's/^Version: //')
GIT_VERSION=$(shell git tag --points-at | sed 's/^v\([0-9.]*\)[a-z]*$$/\1/')

all:

check:
	$(RM) -r $(BIODB_CACHE_DIRECTORY)
	R -q -e "devtools::check('$(CURDIR)')"

vignettes:
	@echo Build vignettes for already installed package, not from local soures.
	R -q -e "devtools::clean_vignettes('$(CURDIR)')"
	R -q -e "devtools::build_vignettes('$(CURDIR)')"

install.deps:
	R -q -e "devtools::install_dev_deps('$(CURDIR)')"

build:
	R -q -e "devtools::build('$(CURDIR)')"

check.version:
	test "$(PKG_VERSION)" = "$(GIT_VERSION)"

test: check.version
	R -q -e "devtools::test('$(CURDIR)', reporter = c('progress', 'fail'))"

install: uninstall install.local list.classes

install.local:
	R --slave -e "devtools::install_local('$(CURDIR)', dependencies = TRUE)"

list.classes:
	R --slave -e 'library(biodb) ; cat("Exported methods and classes:", paste(" ", ls("package:biodb"), collapse = "\\n", sep = ""), sep = "\\n")'

uninstall:
	R --slave -e "try(devtools::uninstall('$(CURDIR)'), silent = TRUE)"

win:
	R -q -e "devtools::build_win('$(CURDIR)')"

conda_install_%: clean
	docker build -t biodb.$@ -f tests/dockerfiles/$@.dockerfile .

clean:
	$(RM) src/*.o src/*.so src/*.dll
	$(RM) -r tests/cache tests/test.log tests/output tests/test\ *.log
	$(RM) -r $(HOME)/.biodb.dev.*.cache

.PHONY: all clean win test check vignettes install uninstall
