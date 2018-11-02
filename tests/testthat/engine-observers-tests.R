# vi: fdm=marker

# Test deprecated methods {{{1
################################################################

test.deprecated.methods <- function(biodb, obs) {

	obs$clearMessages()

	# Load reference entries
	ref.entries <- load.ref.entries('chebi')

	# Get first entry ID
	entry.id <- ref.entries[1, 'accession']

	# Get entry
	entry <- biodb$getFactory()$getEntry('chebi', entry.id)

	# Use deprecated method getFieldCardinality
	card <- entry$getFieldCardinality('name')

	expect_equal(obs$getMsgsByType('caution'), c("Method getFieldCardinality() is now deprecated in ChebiEntry class. Please use now method BiodbEntryField::hasCardOne() or BiodbEntryField::hasCardMany().", "Method getCardinality() is now deprecated in BiodbEntryField class. Please use now method hasCardOne() or hasCardMany()."))
}

# Run observers tests {{{1
################################################################

run.observers.tests <- function(biodb, obs) {

	set.test.context(biodb, "Test observers")

	run.test.that.on.biodb.and.obs("Deprecated methods send correct message.", 'test.deprecated.methods', biodb, obs)
}
