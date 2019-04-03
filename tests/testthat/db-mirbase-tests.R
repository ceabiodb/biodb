# vi: fdm=marker

# Test WS query {{{1
################################################################

test.mirbase.mature.ws.query <- function(db) {

	results <- db$ws.query('onc')
	expect_is(results, 'character')
	expect_length(results, 1)

	results <- db$ws.query('onc', retfmt = 'parsed')
	expect_is(results, 'HTMLInternalDocument')

	results <- db$ws.query('onc', retfmt = 'ids')
	expect_is(results, 'character')
	expect_false(any(is.na(results)))
	expect_length(results, 0)

	results <- db$ws.query('hsa', retfmt = 'ids')
	expect_is(results, 'character')
	expect_false(any(is.na(results)))
	expect_true(length(results) > 0)
}

# Run Mirbase Mature tests {{{1
################################################################

run.mirbase.mature.tests <- function(conn, obs) {
	if (test.online()) {
		test.that('', 'test.mirbase.mature.ws.query', conn = conn)
	}
}
