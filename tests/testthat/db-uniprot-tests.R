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

# Run Uniprot tests {{{1
################################################################

run.uniprot.tests <- function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE)) {
		run.db.test.that('Uniprot entries query works fine with an empty query.', 'test.uniprot.ws.query.empty', db)
		run.db.test.that('Uniprot entries query works fine with multiple columns', 'test.uniprot.ws.query.multiple.columns', db)
		run.db.test.that('Uniprot entries query works fine with a query by name.', 'test.uniprot.ws.query.by.name', db)
	}
}
