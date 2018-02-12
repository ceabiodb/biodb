# vi: fdm=marker

# Test searchCompound {{{1
################################################################

test.searchCompound <- function(db) {

	# Not searchable databases
	not.searchable <- list(molecular.mass = c('chemspider', 'expasy.enzyme'), monoisotopic.mass = c('expasy.enzyme'))

	# Get an entry
	id <- list.ref.entries(db$getId())[[1]]
	expect_true( ! is.null(id))
	expect_length(id, 1)
	entry <- db$getBiodb()$getFactory()$getEntry(db$getId(), id, drop = TRUE)
	expect_true( ! is.null(entry))

	# Search by name
	name <- entry$getFieldValue('name')
	expect_is(name, 'character')
	expect_gt(length(name), 0)
	name <- name[[1]]
	expect_true( ! is.na(name))
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

			# Search by mass
			ids <- db$searchCompound(monoisotopic.mass = monoisotopic.mass, molecular.mass = molecular.mass)
			if (db$getId() %in% not.searchable[[field]])
				expect_null(ids)
			else {
				expect_true( ! is.null(ids))
				expect_true(length(ids) > 0)
				expect_true(id %in% ids)
			}

			# Search by mass and name
			ids <- db$searchCompound(name = name, monoisotopic.mass = monoisotopic.mass, molecular.mass = molecular.mass)
			expect_true( ! is.null(ids))
			expect_true(length(ids) > 0)
			expect_true(id %in% ids)

			# Search by name and slightly different mass
			if (is.null(molecular.mass))
				monoisotopic.mass <- monoisotopic.mass - 0.01
			else
				molecular.mass <- molecular.mass - 0.01
			ids <- db$searchCompound(name = name, monoisotopic.mass = monoisotopic.mass, molecular.mass = molecular.mass, mass.tol = 0.02)
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
