# vi: fdm=marker ts=4 et cc=80

# Test named properties {{{1
################################################################################

test.namedProp <- function(biodb) {
    dbinfo <- biodb::BiodbDbInfo()
}

# Main {{{1
################################################################################

test.that("Named properties work correctly.", 'test.namedProp', biodb = biodb)
