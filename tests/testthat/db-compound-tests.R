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
	mass <- entry$getFieldValue('monoisotopic.mass')
	ids <- db$searchCompound(mass = mass)
	expect_true( ! is.null(ids))
	expect_true(length(ids) > 0)
	expect_true(id %in% ids)

	# Search by exact mass and name
	ids <- db$searchCompound(name = name, mass = mass)
	expect_true( ! is.null(ids))
	expect_true(length(ids) > 0)
	expect_true(id %in% ids)

	# Search by slightly different mass and name
	ids <- db$searchCompound(name = name, mass = mass - 0.1, mass.tol = 0.2)
	expect_true( ! is.null(ids))
	expect_true(length(ids) > 0)
	expect_true(id %in% ids)
}
