# vi: fdm=marker

# Test searchCompound {{{1
################################################################

test.searchCompound <- function(db) {

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
	if (db$isSearchableByField('name')) {
		msg <- paste0('While searching for entry ', id, ' by name "', name, '".')
		expect_true( ! is.null(ids), msg)
		expect_true(length(ids) > 0, msg)
		expect_true(id %in% ids, msg)
	} else
		expect_null(ids)

	# Loop on all mass fields
	for (field in db$getBiodb()$getEntryFields()$getFieldNames(type='mass')) {

		if ( ! entry$hasField(field)) {
			expect_false(db$isSearchableByField(field), paste0('No test for searchCompound() with mass field "', field, '" for database "', db$getId(), '".'))
			next
		}

		mass <- if (entry$hasField(field)) entry$getFieldValue(field) else 10.0
		if (field == 'molecular.mass')
			mass.tol <- 0.01
		else if (mass != floor(mass))
			mass.tol <- 10^-as.integer(nchar(strsplit(as.character(mass), '\\.')[[1]][[2]]))
		else
			mass.tol <- 1.0

		# Search by mass
		max.results <- 3
		ids <- db$searchCompound(mass=mass, mass.tol=mass.tol, mass.field=field, max.results=max.results)
		msg <- paste0('While searching for entry ', id, ' by mass ', mass, ' with mass field ', field, '.')
		if ( ! db$isSearchableByField(field))
			expect_null(ids, msg)
		else {
			expect_true( ! is.null(ids), msg)
			expect_true(length(ids) > 0, msg)
			# Search again if not found with limited max.results
			if ( ! id %in% ids) {
				max.results <- 1000000
				ids <- db$searchCompound(mass=mass, mass.tol=mass.tol, mass.field=field, max.results=max.results)
			}
			expect_true(id %in% ids, msg)
			expect_true(is.na(max.results) || length(ids) <= max.results)
		}

		# Search by mass and name
		if (db$isSearchableByField('name')) {

			# Search by mass and name
			ids <- db$searchCompound(name=name, mass=mass, mass.tol=mass.tol, mass.field=field, max.results=max.results)
			msg <- paste0('While searching for entry ', id, ' by mass ', mass, ' with mass field ', field, ' and by name ', name, '.')
			expect_true( ! is.null(ids), msg)
			expect_true(length(ids) > 0, msg)
			expect_true(id %in% ids, msg)
			expect_true(is.na(max.results) || length(ids) <= max.results)

			# Search by name and slightly different mass
			mass <- mass + mass.tol
			mass.tol <- 2 * mass.tol
			ids <- db$searchCompound(name=name, mass=mass, mass.field=field, mass.tol=mass.tol, max.results=max.results)
			msg <- paste0('While searching for entry ', id, ' by mass ', mass, ' with mass field ', field, ' and by name ', name, '.')
			expect_true( ! is.null(ids), msg)
			expect_true(length(ids) > 0, msg)
			expect_true(id %in% ids, msg)
			expect_true(is.na(max.results) || length(ids) <= max.results)
		}
	}
}

# Test searchCompound() no mass.field {{{1
################################################################

test.searchCompound.no.mass.field <- function(db) {
	expect_error(db$searchCompound(mass=45))
}

# Test annotateMzValues() {{{1
################################################################

test.annotateMzValues <- function(conn) {

	# Mass of a proton
	proton.mass <- 1.0072765

	# Get mass fields
	mass.fields <- conn$getBiodb()$getEntryFields()$getFieldNames('mass')

	# Get entries
	ids <- list.ref.entries(conn$getId())
	entries <- conn$getEntry(ids, drop = FALSE)

	# Loop on mass fields
	for (mf in mass.fields) {

		# Get entries that have a value for this mass field
		ewmf <- entries[vapply(entries, function(e) e$hasField(mf), FUN.VALUE=TRUE)]
		if (length(ewmf) > 0) {

			# Get masses
			masses <- vapply(ewmf, function(e) as.numeric(e$getFieldValue(mf)), FUN.VALUE=1.0)

			# Loop on modes
			for (mode in c('neg', 'pos')) {

				# Compute M/Z values from masses
				mz <- masses + proton.mass * (if (mode == 'neg') -1.0 else +1.0)

				# Loop on mz shifts
				for (shift in c(-0.1, 0.0, 0.1)) {

					# Create input data frame
					df <- data.frame(mz=mz+shift)

					# Annotate
					ret <- conn$annotateMzValues(df, mz.tol=abs(shift)+0.01, mass.field=mf, ms.mode=mode, max.results=3)

					# Test returned value
					if ( ! conn$isSearchableByField(mf))
						testthat::expect_identical(ret, df)
					else {
						testthat::expect_is(ret, 'data.frame')
						testthat::expect_true(all(colnames(df) %in% colnames(ret)))
						testthat::expect_true(nrow(ret) >= nrow(df))
						testthat::expect_true(all(df[['mz']] %in% ret[['mz']]))
						id.col <- paste(conn$getId(), 'accession', sep='.')
						testthat::expect_true(id.col %in% colnames(ret))
					}
				}
			}
		}
	}
}

# Test that we can input a vector in annotateMzValues() {{{1
################################################################################

test_annotateMzValues_input_vector <- function(conn) {

	# Mass of a proton
	proton.mass <- 1.0072765

	# Get mass fields
	mass.fields <- conn$getBiodb()$getEntryFields()$getFieldNames('mass')

	# Get entries
	ids <- list.ref.entries(conn$getId())
	entries <- conn$getEntry(ids, drop = FALSE)

	# Loop on mass fields
	for (mf in mass.fields) {

		if ( ! conn$isSearchableByField(mf))
			next

		# Get entries that have a value for this mass field
		ewmf <- entries[vapply(entries, function(e) e$hasField(mf), FUN.VALUE=TRUE)]
		if (length(ewmf) > 0) {

			# Get masses
			masses <- vapply(ewmf, function(e) as.numeric(e$getFieldValue(mf)), FUN.VALUE=1.0)

			# Annotate
			mz <- masses - proton.mass
			ret <- conn$annotateMzValues(mz, mz.tol=0.01, mass.field=mf, ms.mode='neg', max.results=3)
			testthat::expect_is(ret, 'data.frame')
			id.col <- paste(conn$getId(), 'accession', sep='.')
			testthat::expect_identical(c('mz', id.col), colnames(ret))
		}
	}
}

# Test that we can ask for additional fields in annotateMzValues() {{{1
################################################################################

test_annotateMzValues_additional_fields <- function(conn) {
	testthat::expect_true(FALSE)
}

# Test that PPM tolerance works in annotateMzValues() {{{1
################################################################################

test_annotateMzValues_ppm_tol <- function(conn) {
	testthat::expect_true(FALSE)
}

# Test that input data frame is output untouched in annotateMzValues() {{{1
################################################################################

test_annotateMzValues_input_dataframe_untouched <- function(conn) {
	testthat::expect_true(FALSE)
}

# Main {{{1
################################################################################

if (conn$isCompounddb()) {
	test.that('searchCompound() fails if no mass field is set.', 'test.searchCompound.no.mass.field', conn = conn)
	test.that('We can search for a compound', 'test.searchCompound', conn = conn)
	test.that('annotateMzValues() works correctly.', 'test.annotateMzValues', conn = conn)
	test.that('We can use a single vector as input for annotateMzValues()', 'test_annotateMzValues_input_vector', conn = conn)
	test.that('We can ask for additional fields in annotateMzValues()', 'test_annotateMzValues_additional_fields', conn = conn)
	test.that('Matching with tolerance in ppm works in annotateMzValues()', 'test_annotateMzValues_ppm_tol', conn = conn)
	test.that('Input data frame is output untouched for annotateMzValues()', 'test_annotateMzValues_input_dataframe_untouched', conn = conn)
}
