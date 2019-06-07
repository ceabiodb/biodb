# vi: fdm=marker

# Test deprecated methods {{{1
################################################################

test.deprecatedMethods <- function(biodb, obs) {

	obs$clearMessages()

	# Create database and connector
	id <- 'C1'
	db.df <- rbind(data.frame(), list(accession = id, ms.mode = 'POS', peak.mztheo = 112.07569, peak.comp = 'P9Z6W410 O', peak.attr = '[(M+H)-(H2O)-(NH3)]+', formula = "J114L6M62O2", molecular.mass = 146.10553, name = 'Blablaine'), stringsAsFactors = FALSE)
	conn <- biodb$getFactory()$createConn('mass.csv.file')
	conn$setDb(db.df)

	# Get entry
	entry <- conn$getEntry(id)

	# Use deprecated method getFieldCardinality
	card <- entry$getFieldCardinality('name')

	expect_equal(obs$getMsgsByType('caution'), c("Method getFieldCardinality() is now deprecated in MassCsvFileEntry class. Please use now method BiodbEntryField::hasCardOne() or BiodbEntryField::hasCardMany().", "Method getCardinality() is now deprecated in BiodbEntryField class. Please use now method hasCardOne() or hasCardMany()."))

	# Destroy connector
	biodb$getFactory()$deleteConn(conn$getId())
}

# Main {{{1
################################################################

test.that("Deprecated methods send correct message.", 'test.deprecatedMethods', biodb = biodb, obs = obs)
