# vi: fdm=marker

# Test enzyme-byname ExPASy Enzyme web service {{{1
################################################################

test.wsEnzymeByName <- function(db) {

	ids <- db$wsEnzymeByName("Alcohol", retfmt = 'ids')

	expect_gt(length(ids), 0)
	expect_equal(length(grep('^[0-9.]*$', ids)), length(ids))
}

# Test enzyme-bycomment ExPASy Enzyme web service {{{1
################################################################

test.wsEnzymeByComment <- function(db) {

	ids <- db$wsEnzymeByComment("best", retfmt = 'ids')

	expect_gt(length(ids), 0)
	expect_equal(length(grep('^[0-9.]*$', ids)), length(ids))
}

# Main {{{1
################################################################

test.that('Calls to enzyme-byname ExPASy Enzyme web service work fine.', 'test.wsEnzymeByName', conn = conn)
test.that('Calls to enzyme-bycomment ExPASy Enzyme web service work fine.', 'test.wsEnzymeByComment', conn = conn)
