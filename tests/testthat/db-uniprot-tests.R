# vi: fdm=marker

# Test Uniprot ws.query method {{{1
################################################################

test.uniprot.ws.query <- function(db) {
	result <- db$ws.query(columns = 'id', format = 'tab', limit = 2)
	expect_true( ! is.null(result))
	expect_true( ! is.na(result))
	expect_true(nchar(result) > 0)
}
