# vi: fdm=marker

# Test BiodbCache show {{{1
################################################################

test.BiodbCache.show <- function(biodb) {
	expect_output(biodb$getCache()$show(), regexp = '^Biodb cache .* instance\\..*$')
}

# Test BiodbConfig show {{{1
################################################################

test.BiodbConfig.show <- function(biodb) {
	expect_output(biodb$getConfig()$show(), regexp = '^Biodb config.* instance\\..*Values:.*$')
}

# Test Biodb show {{{1
################################################################

test.Biodb.show <- function(biodb) {
	expect_output(biodb$show(), regexp = '^Biodb instance\\.$')
}

# Test BiodbFactory show {{{1
################################################################

test.BiodbFactory.show <- function(biodb) {
	expect_output(biodb$getFactory()$show(), regexp = '^Biodb factory instance\\.$')
}

# Test BiodbEntry show {{{1
################################################################

test.BiodbEntry.show <- function(biodb) {
	ids <- list.ref.entries('chebi')
	entry <- biodb$getFactory()$getEntry('chebi', ids[[1]])
	expect_output(entry$show(), regexp = '^Biodb .* entry instance .*\\.$')
}

# Test BiodbConn show {{{1
################################################################

test.BiodbConn.show <- function(biodb) {

	# Get connection
	conn <- biodb$getFactory()$getConn('chebi')

	# Test printing
	expect_output(conn$show(), regexp = '^Biodb .* connector instance, using URL .*\\.$')
}

# Test BiodbDbsInfo show {{{1
################################################################

test.BiodbDbsInfo.show <- function(biodb) {
	expect_output(biodb$getDbsInfo()$show(), regexp = '^Biodb databases information instance\\.$')
}

# Test BiodbEntryFields show {{{1
################################################################

test.BiodbEntryFields.show <- function(biodb) {
	expect_output(biodb$getEntryFields()$show(), regexp = '^Biodb entry fields information instance\\.$')
}

# Run object printing tests {{{1
################################################################

run.object.printing.tests <- function(biodb) {

	set.test.context(biodb, "Test object information printing")

	run.test.that.on.biodb("Biodb show method returns correct information.", 'test.Biodb.show', biodb)
	run.test.that.on.biodb("BiodbCache show method returns correct information.", 'test.BiodbCache.show', biodb)
	run.test.that.on.biodb("BiodbConfig show method returns correct information.", 'test.BiodbConfig.show', biodb)
	run.test.that.on.biodb("BiodbFactory show method returns correct information.", 'test.BiodbFactory.show', biodb)
	run.test.that.on.biodb("BiodbEntry show method returns correct information.", 'test.BiodbEntry.show', biodb)
	run.test.that.on.biodb("BiodbConn show method returns correct information.", 'test.BiodbConn.show', biodb)
	run.test.that.on.biodb("BiodbDbsInfo show method returns correct information.", 'test.BiodbDbsInfo.show', biodb)
	run.test.that.on.biodb("BiodbEntryFields show method returns correct information.", 'test.BiodbEntryFields.show', biodb)
}
