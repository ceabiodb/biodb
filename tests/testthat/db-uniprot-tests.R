# vi: fdm=marker

# Test Uniprot ws.query method {{{1
################################################################

test.uniprot.ws.query <- function(db) {
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
