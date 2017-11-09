# vi: fdm=marker

# Test ChEBI encoding issue in XML {{{1
################################################################

test.chebi.encoding.issue.in.xml <- function(db) {

	entry.ids <- db$ws.getLiteEntity(search = "2571", search.category = 'CHEBI ID')
	expect_true( ! is.null(entry.ids))
	expect_true(length(entry.ids) > 0)
	entry <- db$getBiodb()$getFactory()$getEntry(db$getId(), '2571') # +- sign (U+00b1) appears as <c2><b1> if encoding is not set to UTF-8: "<chebiName>(<c2><b1>)-2-Heptanol</chebiName>" instead of "<chebiName>(±)-2-Heptanol</chebiName>"
}

# Test ChEBI searchCompound for bug 20170926.01
################################################################

test.chebi.searchCompound.bug.20170926.01 <- function(db) {

	ids <- db$searchCompound(name = "(gamma)Glu-Leu/Ile", mass = 260.1362)
	expect_true( ! is.null(ids))
	expect_true(length(ids) > 0)
}
