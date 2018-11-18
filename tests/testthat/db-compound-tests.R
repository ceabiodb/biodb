# vi: fdm=marker

# Test searchCompound {{{1
################################################################

test.searchCompound <- function(db) {

	# Not searchable databases
	not.searchable <- list(name = c('hmdb.metabolites'), molecular.mass = c('chemspider', 'expasy.enzyme', 'ncbi.gene'), monoisotopic.mass = c('expasy.enzyme', 'ncbi.gene'), average.mass = c('chemspider'), nominal.mass = c('chemspider'))

	print('-------------------------------- test.searchCompound 01')
	# Get an entry
	id <- list.ref.entries(db$getId())[[1]]
	expect_true( ! is.null(id))
	expect_length(id, 1)
	entry <- db$getBiodb()$getFactory()$getEntry(db$getId(), id, drop = TRUE)
	expect_true( ! is.null(entry))

	print('-------------------------------- test.searchCompound 10')
	# Search by name
	name <- entry$getFieldValue('name')
	expect_is(name, 'character')
	expect_gt(length(name), 0)
	name <- name[[1]]
	expect_true( ! is.na(name))
	ids <- db$searchCompound(name = name)
	print('-------------------------------- test.searchCompound 20')
	if (db$getId() %in% not.searchable$name) {
	print('-------------------------------- test.searchCompound 30')
		expect_null(ids)
	}
	else {
	print('-------------------------------- test.searchCompound 40')
		msg <- paste0('While searching for entry ', id, ' by name "', name, '".')
		expect_true( ! is.null(ids), msg)
		expect_true(length(ids) > 0, msg)
		expect_true(id %in% ids, msg)

		# Loop on all entry fields
		for (field in entry$getFieldNames()) {
	print('-------------------------------- test.searchCompound 50')

			# If this is a mass field
			field.type <- db$getBiodb()$getEntryFields()$get(field)$getType()
			if ( ! is.na(field.type) && field.type == 'mass') {

	print('-------------------------------- test.searchCompound 60')
				mass <- entry$getFieldValue(field)
	print('-------------------------------- test.searchCompound 61')

				# Search by mass
				ids <- db$searchCompound(mass = mass, mass.field = field)
	print('-------------------------------- test.searchCompound 62')
				msg <- paste0('While searching for entry ', id, ' by mass ', mass, ' with mass field ', field, '.')
	print('-------------------------------- test.searchCompound 63')
				if (db$getId() %in% not.searchable[[field]])
					expect_null(ids, msg)
				else {
					expect_true( ! is.null(ids), msg)
					expect_true(length(ids) > 0, msg)
					expect_true(id %in% ids, msg)
				}
	print('-------------------------------- test.searchCompound 70')

				# Search by mass and name
				ids <- db$searchCompound(name = name, mass = mass, mass.field = field)
				msg <- paste0('While searching for entry ', id, ' by mass ', mass, ' with mass field ', field, ' and by name ', name, '.')
				expect_true( ! is.null(ids), msg)
				expect_true(length(ids) > 0, msg)
				expect_true(id %in% ids, msg)
	print('-------------------------------- test.searchCompound 80')

				# Search by name and slightly different mass
				mass <- mass + 0.01
				ids <- db$searchCompound(name = name, mass = mass, mass.field = field, mass.tol = 0.02)
				msg <- paste0('While searching for entry ', id, ' by mass ', mass, ' with mass field ', field, ' and by name ', name, '.')
				expect_true( ! is.null(ids), msg)
				expect_true(length(ids) > 0, msg)
				expect_true(id %in% ids, msg)
	print('-------------------------------- test.searchCompound 90')
			}
	print('-------------------------------- test.searchCompound 98')
		}
	print('-------------------------------- test.searchCompound 99')
	}
	print('-------------------------------- test.searchCompound 100')
}

# Test searchCompound() no mass.field {{{1
################################################################

test.searchCompound.no.mass.field <- function(db) {
	expect_error(db$searchCompound(mass = 45))
}

# Run Compound DB tests {{{1
################################################################

run.compound.db.tests <- function(db, mode) {

	if ( ! methods::is(db, 'RemotedbConn') || mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE))
		if (methods::is(db, 'CompounddbConn')) {
			
			set.test.context(db$getBiodb(), paste("Running compound tests on database", db$getName(), "in", mode, "mode"))

			print('-------------------------------- run.compound.db.tests 01')
			run.db.test.that('searchCompound() fails if no mass field is set.', 'test.searchCompound.no.mass.field', db)
			print('-------------------------------- run.compound.db.tests 02')
			run.db.test.that('We can search for a compound', 'test.searchCompound', db)
			print('-------------------------------- run.compound.db.tests 03')
		}
}
