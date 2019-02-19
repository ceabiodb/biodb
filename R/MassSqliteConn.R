# vi: fdm=marker

#' Class for handling a Mass spectrometry database in SQLite format.
#'
#' @include MassdbConn.R 
#' @include BiodbEditable.R 
#' @include BiodbWritable.R 
MassSqliteConn <- methods::setRefClass('MassSqliteConn', contains = c("MassdbConn", 'BiodbWritable', 'BiodbEditable'), fields = list(.db = "ANY"))

# Constructor {{{1
################################################################

MassSqliteConn$methods( initialize = function(...) { 

	callSuper(...)

	.db <<- NULL
})

# Get entry content {{{1
################################################################

MassSqliteConn$methods( getEntryContent = function(entry.id) {
	
	# Initialize contents to return
	content <- rep(NA_character_, length(entry.id))

	.self$.init.db()

	# TODO

	return(content)
})

# Private methods {{{1
################################################################

# Writable methods {{{2
################################################################

# Do write {{{3
################################################################

MassSqliteConn$methods( .doWrite = function() {

	.self$.init.db()

	# Get new entries
	cached.entries = .self$getAllCacheEntries()
	new.entries = cached.entries[vapply(cached.entries, function(x) x$isNew(), FUN.VALUE = TRUE)]

	if (length(new.entries) > 0) {

		# Start transaction
		DBI::dbBegin(.self$.db)

		# Write into main table
		df = .self$getBiodb()$entriesToDataframe(new.entries, only.card.one = TRUE)
		DBI::dbWriteTable(conn = .self$.db, name = 'entries', value = df, append = TRUE)

		# Loop on all new entries and write other fields to separate tables
		for (entry in new.entries) {

			# Loop on all fields
			for (field.name in entry$getFieldNames()) {

				field = .self$getBiodb()$getEntryFields()$get(field.name)

				# Write data frame field
				if (field$getClass() == 'data.frame')
					DBI::dbWriteTable(conn = .self$.db, name = field.name, value = cbind(accession = entry$getFieldValue('accession'), entry$getFieldValue(field.name)), append = TRUE)

				# Write multiple values field
				else if (field$hasCardMany()) {
					values = list(accession = entry$getFieldValue('accession'))
					values[[field.name]] = entry$getFieldValue(field.name)
					DBI::dbWriteTable(conn = .self$.db, name = field.name, value = as.data.frame(values), append = TRUE)
				}
			}
		}

		# Commit transaction
		DBI::dbCommit(.self$.db)

		# Unset "new" flag
		lapply(new.entries, function(x) x$.setAsNew(FALSE))
	}
})

# Init db {{{2
################################################################

MassSqliteConn$methods( .init.db = function() {

	if (is.null(.self$.db))
		.db <<-  DBI::dbConnect(RSQLite::SQLite(), dbname = .self$getUrl('base.url'))
})

# Do terminate {{{2
################################################################

MassSqliteConn$methods( .doTerminate = function() {

	if ( ! is.null(.self$.db)) {
		DBI::dbDisconnect(.self$.db)
		.db <<- NULL
	}
})
