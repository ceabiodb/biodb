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
	mass <- NA
	if (entry$hasField('monoisotopic.mass'))
		mass <- entry$getFieldValue('monoisotopic.mass')
	else if (entry$hasField('mass'))
		mass <- entry$getFieldValue('mass')
	if ( ! is.na(mass)) {
		ids <- db$searchCompound(mass = mass)
		expect_true( ! is.null(ids))
		expect_true(length(ids) > 0)
		expect_true(id %in% ids)
	}

	# Search by exact mass and name
	if ( ! is.na(mass)) {
		ids <- db$searchCompound(name = name, mass = mass)
		expect_true( ! is.null(ids))
		expect_true(length(ids) > 0)
		expect_true(id %in% ids)
	}

	# Search by slightly different mass and name
	if ( ! is.na(mass)) {
		ids <- db$searchCompound(name = name, mass = mass - 0.1, mass.tol = 0.2)
		expect_true( ! is.null(ids))
		expect_true(length(ids) > 0)
		expect_true(id %in% ids)
	}
}

# Run Compound DB tests {{{1
################################################################

run.compound.db.tests <- function(db, mode) {
	if ( ! methods::is(db, 'RemotedbConn') || mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE))
		if (methods::is(db, 'CompounddbConn'))
			run.db.test.that('We can search for a compound', 'test.searchCompound', db)
}
