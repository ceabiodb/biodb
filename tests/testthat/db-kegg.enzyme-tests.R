# vi: fdm=marker ts=4 et cc=80 

# Test KEGG Enzyme getPathwayIds() {{{1
################################################################

test.kegg.enzyme.getPathwayIds = function(conn) {
	c = '1.2.1.3'
	ids = conn$getPathwayIds(c, 'mmu')
	testthat::expect_is(ids, 'character')
	testthat::expect_true(length(ids) > 0)
}

# Run KEGG Enzyme tests {{{1
################################################################

run.kegg.enzyme.tests <- function(conn, obs) {
	if (test.online()) {
		test.that('getPathwayIds() works correctly.',
                  'test.kegg.enzyme.getPathwayIds', conn = conn)
    }
}
