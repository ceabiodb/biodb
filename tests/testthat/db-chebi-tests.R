# vi: fdm=marker

# Test ChEBI encoding issue in XML {{{1
################################################################

test.chebi.encoding.issue.in.xml <- function(db) {

	entry.ids <- db$ws.getLiteEntity(search = "2571", search.category = 'CHEBI ID')
	expect_true( ! is.null(entry.ids))
	expect_true(length(entry.ids) > 0)
	entry <- db$getBiodb()$getFactory()$getEntry(db$getId(), '2571') # +- sign (U+00b1) appears as <c2><b1> if encoding is not set to UTF-8: "<chebiName>(<c2><b1>)-2-Heptanol</chebiName>" instead of "<chebiName>(±)-2-Heptanol</chebiName>"
}

# Run ChEBI tests {{{1
################################################################

run.chebi.tests <- function(conn, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE)) {
		test.that('ChEBI encoding issue in XML is handled.', 'test.chebi.encoding.issue.in.xml', conn = conn)
	}
}
