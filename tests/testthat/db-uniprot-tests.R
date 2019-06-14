# vi: fdm=marker

# Test Uniprot wsQuery method with empty query {{{1
################################################################

test.uniprot.wsQuery.empty <- function(db) {
	n <- 2
	result <- db$wsQuery(columns = 'id', format = 'tab', limit = n)
	expect_true( ! is.null(result))
	expect_true( ! is.na(result))
	expect_true(nchar(result) > 0)
	readtc <- textConnection(result, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", header = TRUE)
	expect_true(colnames(df) == 'Entry')
	expect_true(nrow(df) == n)
}

# Test Uniprot wsQuery method with query by name {{{1
################################################################

test.uniprot.wsQuery.by.name <- function(db) {
	n <- 2
	result <- db$wsQuery(query = 'name:"prion protein"', columns = 'id', format = 'tab', limit = n)
	expect_true( ! is.null(result))
	expect_true( ! is.na(result))
	expect_true(nchar(result) > 0)
	readtc <- textConnection(result, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", header = TRUE)
	expect_true(colnames(df) == 'Entry')
	expect_true(nrow(df) == n)
}

# Test Uniprot wsQuery method with empty query and multiple columns {{{1
################################################################

test.uniprot.wsQuery.multiple.columns <- function(db) {
	n <- 2
	results <- db$wsQuery(columns = c('id', 'entry name'), format = 'tab', limit = n, retfmt = 'parsed')
	testthat::expect_is(results, 'data.frame')
	testthat::expect_true(all(c('Entry', 'Entry name') %in% colnames(results)))
	testthat::expect_true(nrow(results) == n)
}

# Main {{{1
################################################################

test.that('Uniprot entries query works fine with an empty query.', 'test.uniprot.wsQuery.empty', conn = conn)
test.that('Uniprot entries query works fine with multiple columns', 'test.uniprot.wsQuery.multiple.columns', conn = conn)
test.that('Uniprot entries query works fine with a query by name.', 'test.uniprot.wsQuery.by.name', conn = conn)
