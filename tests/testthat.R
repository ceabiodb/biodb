# vi: fdm=marker
# Script needed to run testthat automatically from ‘R CMD check’. See testthat::test_dir documentation.
library(testthat)
library(biodb)

# Constants {{{1
################################################################

ENV = Sys.getenv()
WORKING_DIR=getwd() # `R CMD check` run tests inside folder `biodb.Rcheck/tests`.
SRC_DIR=file.path(WORKING_DIR, '..', '00_pkg_src', 'biodb') # `R CMD check` places package sources inside folder `biodb.Rcheck/00_pkg_src/biodb`.
README_PATH=file.path(SRC_DIR, 'README.md')

# Set databases to test {{{1
################################################################

# If no cache folder is defined, and no databases are set for testing, then we just test non-remote databases:
if (all( ! c('DATABASES', 'BIODB_CACHE_DIRECTORY') %in% names(ENV)))
	Sys.setenv(DATABASES = "mass.csv.file,mass.sqlite")

# Print env vars {{{1
################################################################

ENV = Sys.getenv() # Reload env vars
cat("ENVIRONMENT VARIABLES:\n")
vars = grep('^(BIODB_.*|DATABASES)$', names(ENV), value = TRUE, perl = TRUE)
print(ENV[vars])

# Run tests {{{1
################################################################

Sys.setenv(TESTTHAT_REPORTER = "summary")
test_check("biodb")
