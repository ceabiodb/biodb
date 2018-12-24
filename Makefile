export BIODB_CACHE_DIRECTORY=$(HOME)/.biodb.dev.check.cache
PKG_VERSION=$(shell grep '^Version:' DESCRIPTION | sed 's/^Version: //')
GIT_VERSION=$(shell git describe --tags | sed 's/^v\([0-9.]*\)[a-z]*.*$$/\1/')
ZIPPED_PKG=biodb_$(PKG_VERSION).tar.gz

all:

check: devtools.check

devtools.check: clean.cache
	R -q -e "results <- devtools::check('$(CURDIR)') ; if (length(results$$errors) > 0 || length(results$$warnings) > 0 || length(results$$notes) > 0) quit(status = 1)"

r.check: clean.cache r.build
	R CMD check --as-cran $(ZIPPED_PKG)

vignettes:
	@echo Build vignettes for already installed package, not from local soures.
	R -q -e "devtools::clean_vignettes('$(CURDIR)')"
	R -q -e "devtools::build_vignettes('$(CURDIR)')"

install.deps:
	R -q -e "devtools::install_dev_deps('$(CURDIR)')"

doc:
	R -q -e "devtools::document('$(CURDIR)')"

build: devtools.build

devtools.build:
	R -q -e "devtools::build('$(CURDIR)')"

r.build: doc $(ZIPPED_PKG)

$(ZIPPED_PKG):
	cd "$(dir $(CURDIR))" && R CMD build "$(notdir $(CURDIR))" && mv "$(ZIPPED_PKG)" "$(CURDIR)"

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

clean: clean.cache clean.build
	$(RM) src/*.o src/*.so src/*.dll
	$(RM) -r tests/test.log tests/output tests/test\ *.log

clean.build:
	$(RM) biodb_*.tar.gz

clean.cache:
	$(RM) -r $(BIODB_CACHE_DIRECTORY)

.PHONY: all clean win test check vignettes install uninstall devtools.check r.check devtools.build r.build clean.build clean.cache doc
