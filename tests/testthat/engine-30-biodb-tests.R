# vi: fdm=marker

# Test convertEntryIdFieldToDbClass() {{{1
################################################################

test.convertEntryIdFieldToDbClass <- function(biodb, obs) {

	# Check all databases
	for (db in biodb$getDbsInfo()$getAll())
		testthat::expect_equal(biodb$convertEntryIdFieldToDbClass(db$getEntryIdField()), db$getDbClass())

	# Check a wrong database name
	testthat::expect_null(biodb$convertEntryIdFieldToDbClass('chebi'))
	testthat::expect_null(biodb$convertEntryIdFieldToDbClass('blabla.id'))
}

# Main {{{1
################################################################

test.that("convertEntryIdFieldToDbClass() works correctly.", 'test.convertEntryIdFieldToDbClass', biodb = biodb, obs = obs)
