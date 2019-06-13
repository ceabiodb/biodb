# vi: fdm=marker

# Test searchCompound {{{1
################################################################

test.searchCompound <- function(db) {

	# Not searchable databases
	not.searchable <- list(name = c('hmdb.metabolites'), molecular.mass = c('chemspider', 'expasy.enzyme', 'ncbi.gene', 'peakforest.compound'), monoisotopic.mass = c('expasy.enzyme', 'ncbi.gene'), average.mass = c('chemspider'), nominal.mass = c('chemspider', 'peakforest.compound'))

	# Get an entry
	id <- list.ref.entries(db$getId())[[1]]
	expect_true( ! is.null(id))
	expect_length(id, 1)
	entry <- db$getEntry(id, drop = TRUE)
	expect_true( ! is.null(entry))

	# Search by name
	name <- entry$getFieldValue('name')
	expect_is(name, 'character')
	expect_gt(length(name), 0)
	name <- name[[1]]
	expect_true( ! is.na(name))
	ids <- db$searchCompound(name = name)
	if (db$getId() %in% not.searchable$name) {
		expect_null(ids)
	}
	else {
		msg <- paste0('While searching for entry ', id, ' by name "', name, '".')
		expect_true( ! is.null(ids), msg)
		expect_true(length(ids) > 0, msg)
		expect_true(id %in% ids, msg)

		# Loop on all entry fields
		for (field in entry$getFieldNames()) {

			# If this is a mass field
			field.type <- db$getBiodb()$getEntryFields()$get(field)$getType()
			if ( ! is.na(field.type) && field.type == 'mass') {

				mass <- entry$getFieldValue(field)
				if (mass != floor(mass))
					mass.tol = 10^-as.integer(nchar(strsplit(as.character(mass), '\\.')[[1]][[2]]))
				else
					mass.tol = 1

				# Search by mass
				ids <- db$searchCompound(mass = mass, mass.tol = mass.tol, mass.field = field)
				msg <- paste0('While searching for entry ', id, ' by mass ', mass, ' with mass field ', field, '.')
				if (db$getId() %in% not.searchable[[field]])
					expect_null(ids, msg)
				else {
					expect_true( ! is.null(ids), msg)
					expect_true(length(ids) > 0, msg)
					expect_true(id %in% ids, msg)
				}

				# Search by mass and name
				ids <- db$searchCompound(name = name, mass = mass, mass.tol = mass.tol, mass.field = field)
				msg <- paste0('While searching for entry ', id, ' by mass ', mass, ' with mass field ', field, ' and by name ', name, '.')
				expect_true( ! is.null(ids), msg)
				expect_true(length(ids) > 0, msg)
				expect_true(id %in% ids, msg)

				# Search by name and slightly different mass
				mass <- mass + mass.tol
				ids <- db$searchCompound(name = name, mass = mass, mass.field = field, mass.tol = mass.tol)
				msg <- paste0('While searching for entry ', id, ' by mass ', mass, ' with mass field ', field, ' and by name ', name, '.')
				expect_true( ! is.null(ids), msg)
				expect_true(length(ids) > 0, msg)
				expect_true(id %in% ids, msg)
			}
		}
	}
}

# Test searchCompound() no mass.field {{{1
################################################################

test.searchCompound.no.mass.field <- function(db) {
	expect_error(db$searchCompound(mass = 45))
}

# Main {{{1
################################################################

if (conn$isCompounddb()) {
	test.that('searchCompound() fails if no mass field is set.', 'test.searchCompound.no.mass.field', conn = conn)
	test.that('We can search for a compound', 'test.searchCompound', conn = conn)
}
