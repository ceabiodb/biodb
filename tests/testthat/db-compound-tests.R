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

	# Search by mass
	mass <- entry$getFieldValue('mass')
	ids <- db$searchCompound(mass = mass)
	expect_true( ! is.null(ids))
	expect_true(length(ids) > 0)
}
