# Script needed to run testthat automatically from ‘R CMD check’. See
# testthat::test_dir documentation.
library(testthat)
library({{pkgName}})
Sys.setenv(TESTTHAT_REPORTER="summary")
test_check("{{pkgName}}")
