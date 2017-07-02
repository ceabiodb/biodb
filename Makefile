all:

check:
	@R -q -e "devtools::check('$(CURDIR)')"

#vignettes:
#	@R -q -e "devtools::install_local('$(CURDIR)')" # Install package
#	@R -q -e "devtools::build_vignettes('$(CURDIR)')" # Build vignettes for already installed package, not from local soures.

build:
	@R -q -e "devtools::build('$(CURDIR)')"

test:
	@R -q -e "devtools::test('$(CURDIR)')"

install:
	@R -q -e "try(devtools::uninstall('$(CURDIR)'), silent = TRUE) ; devtools::install_local('$(CURDIR)') ; library(biodb) ; cat('***** Exported methods and classes:', paste(ls('package:biodb'), collapse = ', '), \".\\\\n\", sep = '')"

win:
	@R -q -e "devtools::build_win('$(CURDIR)')"

clean:
	$(RM) src/*.o src/*.so src/*.dll

.PHONY: all clean win test check vignettes
