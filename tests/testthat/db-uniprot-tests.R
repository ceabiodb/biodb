# vi: fdm=marker

# Test Uniprot ws.query method with empty query {{{1
################################################################

test.uniprot.ws.query.empty <- function(db) {
	n <- 2
	result <- db$ws.query(columns = 'id', format = 'tab', limit = n)
	expect_true( ! is.null(result))
	expect_true( ! is.na(result))
	expect_true(nchar(result) > 0)
	readtc <- textConnection(result, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", header = TRUE)
	expect_true(colnames(df) == c('Entry'))
	expect_true(nrow(df) == n)
}

# Test Uniprot ws.query method with query by name {{{1
################################################################

test.uniprot.ws.query.by.name <- function(db) {
	n <- 2
	result <- db$ws.query(query = 'name:"prion protein"', columns = 'id', format = 'tab', limit = n)
	expect_true( ! is.null(result))
	expect_true( ! is.na(result))
	expect_true(nchar(result) > 0)
	readtc <- textConnection(result, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", header = TRUE)
	expect_true(colnames(df) == c('Entry'))
	expect_true(nrow(df) == n)
}

# Test Uniprot ws.query method with empty query and multiple columns {{{1
################################################################

test.uniprot.ws.query.multiple.columns <- function(db) {
	n <- 2
	result <- db$ws.query(columns = c('id', 'entry name'), format = 'tab', limit = n)
	expect_true( ! is.null(result))
	expect_true( ! is.na(result))
	expect_true(nchar(result) > 0)
	readtc <- textConnection(result, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", header = TRUE)
	expect_true(identical(colnames(df), c('Entry', 'Entry.name')))
	expect_true(nrow(df) == n)
}
