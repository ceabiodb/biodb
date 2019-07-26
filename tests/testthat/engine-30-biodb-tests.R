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

# Test collapseRows() {{{1
################################################################

test.collapseRows <- function(biodb, obs) {

	# Basic tests
	testthat::expect_null(biodb$collapseRows(NULL))
	testthat::expect_identical(data.frame(), biodb$collapseRows(data.frame()))
	testthat::expect_identical(data.frame(a=1), biodb$collapseRows(data.frame(a=1)))
	testthat::expect_identical(data.frame(a=c(1,2)), biodb$collapseRows(data.frame(a=c(1,2))))
	testthat::expect_identical(data.frame(a=c(1,2)), biodb$collapseRows(data.frame(a=c(1,2,2))))
	testthat::expect_identical(data.frame(a=c(1,2),b=c('5','5|7'), stringsAsFactors=FALSE), biodb$collapseRows(data.frame(a=c(1,2,2),b=c(5,5,7))))

	# Test with a different separator
	testthat::expect_identical(data.frame(a=c(1,2),b=c('5','5;7'), stringsAsFactors=FALSE), biodb$collapseRows(data.frame(a=c(1,2,2),b=c(5,5,7)), sep=';'))

	# Test with NA values
	testthat::expect_identical(data.frame(a=c(1,NA,NA,2),b=c('5',NA,6,'5|7'), stringsAsFactors=FALSE), biodb$collapseRows(data.frame(a=c(1,NA,NA,2,2),b=c(5,NA,6,5,7))))
}

# Main {{{1
################################################################

test.that("convertEntryIdFieldToDbClass() works correctly.", 'test.convertEntryIdFieldToDbClass', biodb = biodb, obs = obs)
test.that('collapseRows() works correctly.', 'test.collapseRows', biodb = biodb, obs = obs)
