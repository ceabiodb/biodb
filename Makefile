all:

check:
	@R -q -e "devtools::check('$(CURDIR)')"

vignettes: install
	@R -q -e "devtools::build_vignettes('$(CURDIR)')" # Build vignettes for already installed package, not from local soures.

build:
	@R -q -e "devtools::build('$(CURDIR)')"

test:
	@R -q -e "devtools::test('$(CURDIR)')"

install:
	@R -q -e "try(devtools::uninstall('$(CURDIR)'), silent = TRUE) ; devtools::install_local('$(CURDIR)') ; library(biodb) ; cat('***** Exported methods and classes: ', paste(ls('package:biodb'), collapse = ', '), \".\\\\n\", sep = '')"

win:
	echo "Sending request for testing on Windows platform..."
	@R -q -e "devtools::build_win('$(CURDIR)')"

ex:
	echo "Testing examples..."
	for ex in examples/*.R ; do Rscript $$ex || exit 1 ; done

exdock:
	echo "Testing on a bare Linux system (no SVN or UNZIP installed)..."
	docker build -t biodb-bare-r -f bare-r.dockerfile .
	for ex in examples/*.R ; do docker run -v $(PWD)/examples:/examples biodb-bare-r /$$ex || exit 1 ; done

clean:
	$(RM) src/*.o src/*.so src/*.dll
	$(RM) -r tests/cache tests/test.log tests/output

.PHONY: all clean win test check vignettes ex exdock
