# Script needed to run testthat automatically from ‘R CMD check’. See testthat::test_dir documentation.
library(testthat)
library(biodb)

test_check("biodb")
