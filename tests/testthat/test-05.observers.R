# vi: fdm=marker

source('common.R')

# Test deprecated methods {{{1
################################################################

test.deprecated.methods <- function(biodb) {

	# Create test observer class
	TestObs <- methods::setRefClass("TestObs", contains = "BiodbObserver", fields = list(msgs = 'character'))
	TestObs$methods( initialize = function(...) {
		msgs <<- character(0)
	})
	TestObs$methods( message = function(type = MSG.INFO, msg, class = NA_character_, method = NA_character_, level = 1) {
		if (type == MSG.CAUTION)
			msgs <<- c(msgs, msg)
	})
	obs <- TestObs$new()

	# Set logger
	biodb$addObservers(obs)

	# Load reference entries
	ref.entries <- load.ref.entries(BIODB.CHEBI)

	# Get first entry ID
	entry.id <- ref.entries[1, 'accession']

	# Get entry
	entry <- biodb$getFactory()$getEntry(BIODB.CHEBI, entry.id)

	# Use deprecated method getFieldCardinality
	card <- entry$getFieldCardinality('name')

	expect_equal(obs$msgs, c("Method getFieldCardinality() is now deprecated in ChebiEntry class. Please use now method BiodbEntryFields::hasCardOne() or BiodbEntryFields::hasCardMany().", "Method getCardinality() is now deprecated in BiodbEntryField class. Please use now method hasCardOne() or hasCardMany()."))
}

# MAIN {{{1
################################################################

biodb <- Biodb$new(logger = FALSE)
set.test.context(biodb, "Test observers")
set.mode(biodb, MODE.OFFLINE)
test_that("Deprecated methods send correct message.", test.deprecated.methods(biodb))
