# vi: fdm=marker ts=4 et cc=80 

# Test KEGG Enzyme getPathwayIds() {{{1
################################################################################

test.kegg.enzyme.getPathwayIds = function(conn) {
    c = '1.2.1.3'
    ids = conn$getPathwayIds(c, 'mmu')
    testthat::expect_is(ids, 'character')
    testthat::expect_true(length(ids) > 0)
}

# Test issue 340 {{{1
################################################################################

test.kegg.issue_340 <- function(conn) {
    
    # Check that the right pathways are returned by getPathwaysIds()
    enz <- '1.14.14.1'
    org <- 'mmu'
    
    # Get pathways
    pws <- conn$getPathwayIds(enz, org = org)
    
    # Define wrong & right pathways to check
    wrong_pws <- c('mmu04913', 'mmu00627', 'mmu01110', 'mmu01120')
    right_pws <- c(
        'mmu00071',
        'mmu00140',
        'mmu00232',
        'mmu00380',
        'mmu00590',
        'mmu00591',
        'mmu00830',
        'mmu00980',
        'mmu00982',
        'mmu01100')
    
    # Check
    testthat::expect_false(any(wrong_pws %in% pws))
    testthat::expect_true(all(right_pws %in% pws))
}

# Main {{{1
################################################################################

test.that('getPathwayIds() works correctly.',
          'test.kegg.enzyme.getPathwayIds', conn = conn)
test.that('issue 340 is corrected.',
          'test.kegg.issue_340', conn = conn)
