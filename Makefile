all:

check:
	R -q -e "devtools::check('$(CURDIR)')"

vignettes: install
	R -q -e "devtools::clean_vignettes('$(CURDIR)')"
	R -q -e "devtools::build_vignettes('$(CURDIR)')" # Build vignettes for already installed package, not from local soures.

build:
	R -q -e "devtools::build('$(CURDIR)')"

test:
	R -q -e "devtools::test('$(CURDIR)')"

install: uninstall install.local list.classes

install.local:
	R --slave -e "devtools::install_local('$(CURDIR)')"

list.classes:
	R --slave -e 'library(biodb) ; cat("Exported methods and classes:", paste(" ", ls("package:biodb"), collapse = "\\n", sep = ""), sep = "\\n")'

uninstall:
	R --slave -e "try(devtools::uninstall('$(CURDIR)'), silent = TRUE)"

win:
	R -q -e "devtools::build_win('$(CURDIR)')"

ex:
	echo "Testing examples..."
	for ex in examples/*.R ; do echo "Running $$ex..." ; Rscript $$ex >$$ex.output 2>$$ex.err || exit 1 ; done

exdock: clean
	echo "Testing on a bare Linux system (no SVN or UNZIP installed)..."
	docker build -t biodb-bare-r -f bare-r.dockerfile .
	for ex in examples/*.R ; do echo "Running $$ex..." ; docker run -v $(PWD)/examples:/examples biodb-bare-r /$$ex >$$ex-docker.output 2>$$ex-docker.err || exit 1 ; done

clean:
	$(RM) src/*.o src/*.so src/*.dll
	$(RM) -r tests/cache tests/test.log tests/output

.PHONY: all clean win test check vignettes ex exdock install uninstall
