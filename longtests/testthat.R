# vi: fdm=marker
# Script needed to run testthat automatically from ‘R CMD check’. See
# testthat::test_dir documentation.
library(testthat)
library(biodb)
Sys.setenv(TESTTHAT_REPORTER="summary")
Sys.setenv(BIODB_LOG_DST="console")
test_check("biodb")
