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

windows-test:
	echo "Sending request for testing on Windows platform..."
	@R -q -e "devtools::build_win('$(CURDIR)')"

bare-test:
	echo "Testing on a bare Linux system (no SVN or UNZIP installed)..."
	docker build -t biodb-bare-test dockers/bare-test

clean:
	$(RM) src/*.o src/*.so src/*.dll

.PHONY: all clean win test check vignettes bare
