# vi: fdm=marker

# Test connector already exists {{{1
################################################################

test.connectorAlreadyExists <- function(biodb, obs) {

	# Test with Mass CSV file
	conn <- biodb$getFactory()$createConn('mass.csv.file', url = MASSFILEDB.URL)
	testthat::expect_error(biodb$getFactory()$createConn('mass.csv.file', url = MASSFILEDB.URL))

	# Make change to path
	same.url <- normalizePath(MASSFILEDB.URL, mustWork = FALSE)
	same.url <- file.path(dirname(dirname(same.url)), '..', basename(dirname(dirname(same.url))), basename(dirname(same.url)), basename(same.url))
	testthat::expect_error(biodb$getFactory()$createConn('mass.csv.file', url = same.url))
	expect_match(obs$lastMsg(), '^A connector \\(mass\\.csv\\.file\\) already exists for database mass\\.csv\\.file .*$', perl = TRUE)
	conn.2 <- biodb$getFactory()$createConn('mass.csv.file', url = same.url, fail.if.exists = FALSE)
	expect_is(conn.2, 'BiodbConn')
	expect_match(obs$getLastMsgByType('caution'), "A connector \\(mass\\.csv\\.file\\) already exists for database mass\\.csv\\.file with the same URL ", , perl = TRUE)

	# Test with ChEBI
	biodb$getFactory()$deleteConn(db.class = 'chebi')
	conn.chebi <- biodb$getFactory()$createConn('chebi')
	testthat::expect_error(biodb$getFactory()$createConn('chebi'))
	expect_match(obs$lastMsg(), "^A connector \\(chebi\\) already exists for database chebi with the same URL .*$", perl = TRUE)
	conn.chebi.2 <- biodb$getFactory()$createConn('chebi', fail.if.exists = FALSE)
	expect_is(conn.chebi.2, 'BiodbConn')
	expect_match(obs$getLastMsgByType('caution'), "^A connector \\(chebi\\) already exists for database chebi with the same URL .*$", perl = TRUE)
}

# Run factory tests {{{1
################################################################

run.factory.tests <- function(biodb, obs) {

	set.test.context(biodb, "Test factory")

	run.test.that.on.biodb.and.obs("We detect when an identical connector already exists.", 'test.connectorAlreadyExists', biodb, obs)
}
