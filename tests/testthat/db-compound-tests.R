# vi: fdm=marker

# Test searchCompound {{{1
################################################################

test.searchCompound <- function(db) {

	# Get an entry
	id <- db$getEntryIds(max.result = 1)
	expect_true( ! is.null(id))
	entry <- db$getBiodb()$getFactory()$getEntry(db$getId(), id)
	expect_true( ! is.null(entry))

	# Search by name
	name <- entry$getFieldValue('name')
	ids <- db$searchCompound(name = name)
	expect_true( ! is.null(ids))
	expect_true(length(ids) > 0)
	expect_true(id %in% ids)

	# Search by mass
	for (field in c('monoisotopic.mass', 'molecular.mass')) {
		molecular.mass <- NULL
		monoisotopic.mass <- NULL
		if (field == 'monoisotopic.mass' && entry$hasField('monoisotopic.mass'))
			monoisotopic.mass <- entry$getFieldValue('monoisotopic.mass')
		else if (field == 'molecular.mass' && entry$hasField('molecular.mass'))
			molecular.mass <- entry$getFieldValue('molecular.mass')
		if ( ! is.null(monoisotopic.mass) || ! is.null(molecular.mass)) {
			ids <- db$searchCompound(monoisotopic.mass = monoisotopic.mass, molecular.mass = molecular.mass)
			expect_true( ! is.null(ids))
			expect_true(length(ids) > 0)
			expect_true(id %in% ids)

			# Search by exact mass and name
			ids <- db$searchCompound(name = name, monoisotopic.mass = monoisotopic.mass, molecular.mass = molecular.mass)
			expect_true( ! is.null(ids))
			expect_true(length(ids) > 0)

			# Search by name and slightly different mass
			if (is.null(molecular.mass))
				monoisotopic.mass <- monoisotopic.mass - 0.1
			else
				molecular.mass <- molecular.mass - 0.1
			ids <- db$searchCompound(name = name, monoisotopic.mass = monoisotopic.mass, molecular.mass = molecular.mass, mass.tol = 0.2)
			expect_true( ! is.null(ids))
			expect_true(length(ids) > 0)
			expect_true(id %in% ids)
		}
	}
}

# Run Compound DB tests {{{1
################################################################

run.compound.db.tests <- function(db, mode) {
	if ( ! methods::is(db, 'RemotedbConn') || mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE))
		if (methods::is(db, 'CompounddbConn')) {
			
			set.test.context(db$getBiodb(), paste("Running compound tests on database", db$getId(), "in", mode, "mode"))

			run.db.test.that('We can search for a compound', 'test.searchCompound', db)
		}
}
