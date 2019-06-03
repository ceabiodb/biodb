# vi: fdm=marker

# Test ChEBI encoding issue in XML {{{1
################################################################

test.chebi.encoding.issue.in.xml <- function(conn) {

	entry.ids <- conn$wsGetLiteEntity(search = "2571", search.category = 'CHEBI ID', retfmt = 'ids')
	testthat::expect_is(entry.ids, 'character')
	testthat::expect_length(entry.ids, 1)
	entry <- conn$getEntry('2571') # +- sign (U+00b1) appears as <c2><b1> if encoding is not set to UTF-8: "<chebiName>(<c2><b1>)-2-Heptanol</chebiName>" instead of "<chebiName>(±)-2-Heptanol</chebiName>"
}

# Test ChEBI wsGetLiteEntity() {{{1
################################################################

test.chebi.wsGetLiteEntity <- function(conn) {

	# Get Id
	id = "2571"
	entry.ids = conn$wsGetLiteEntity(search = id, search.category = 'CHEBI ID', retfmt = 'ids')
	testthat::expect_is(entry.ids, 'character')
	testthat::expect_length(entry.ids, 1)
	testthat::expect_identical(entry.ids, id)
}

# Run ChEBI tests {{{1
################################################################

run.chebi.tests <- function(conn, obs) {
	if (test.online()) {
		test.that('Web service getLiteEntity works fine.', 'test.chebi.wsGetLiteEntity', conn = conn)
		test.that('ChEBI encoding issue in XML is handled.', 'test.chebi.encoding.issue.in.xml', conn = conn)
	}
}
