# vi: fdm=marker

#' Class for handling a Mass spectrometry database in SQLite format.
#'
#' @include MassdbConn.R 
#' @include BiodbEditable.R 
#' @include BiodbWritable.R 
MassSqliteConn <- methods::setRefClass('MassSqliteConn', contains = c("MassdbConn", 'BiodbWritable', 'BiodbEditable'), fields = list(.db = "ANY", .tables = "character"))

# Constructor {{{1
################################################################

MassSqliteConn$methods( initialize = function(...) { 

	callSuper(...)

	.db <<- NULL
	.tables <<- character()
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

	# Loop on all new entries
	for (entry in .self$getAllCacheEntries())
		if (entry$isNew()) {

			# Start transaction
			DBI::dbBegin(.self$.db)

			single.value.fields = list()

			# Loop on all fields
			for (field.name in entry$getFieldNames()) {

				field = .self$getBiodb()$getEntryFields()$get(field.name)
				accession = entry$getFieldValue('accession')
				value = entry$getFieldValue(field.name)

				# Write data frame field
				if (field$getType() == 'data.frame')
					.self$.write.data.frame.field(accession, name = field.name, value = value)

				# Write multiple values field
				else if (field$hasCardOne())
					.self$.write.multiple.values.field(accession, name = field.name, value = value)

				# Write single value field
				else
					single.value.fields[[field.name]] = value
			}

			.self$.write.single.value.fields(single.value.fields)

			# Commit transaction
			DBI::dbCommit(.self$.db)

			# Unset "new" flag
			entry$.setAsNew(FALSE)
		}
})

# Write single value fields {{{2
################################################################

MassSqliteConn$methods( .write.single.value.fields = function(fields) {

	# Make sure table exists
	.self$.create.table('entries')

	# Make sure columns exist
	for (col in names(fields))
		.self$.create.column(table = 'entries', name = col, type = class(fields[[col]]))

	# Add row
	.self$.insert.values(table = 'entries', values)
})

# Insert values {{{2
################################################################

MassSqliteConn$methods( .insert.values = function(table, columns, values) {

	# Build query
	cols = paste(columns, collapse = ', ')
	vals = paste(values, collapse = ', ')
	query = paste0('insert into entries (', cols, ') values (', vals, ');')
	DBI::dbSendQuery(.self$.db, query)
})

# Init db {{{2
################################################################

MassSqliteConn$methods( .init.db = function() {

	if (is.null(.self$.db))
		.db <<-  DBI::dbConnect(RSQLite::SQLite(), dbname = .self$getUrl('base.url'))
})

# Create table {{{2
################################################################

MassSqliteConn$methods( .create.table = function(name) {

	# Get list of existing tables
	if (is.null(.self$.tables))
		.tables <<- DBI::dbListTables(.self$.db)

	# Create tables
	if ( ! name %in% .self$.tables) {
		DBI::sqlCreateTable(.self$.db, table = name, fields = c(accession = 'text'), row.names = FALSE)
		.table <<- c(.self$.tables, name)
	}
})

# Do terminate {{{2
################################################################

MassSqliteConn$methods( .doTerminate = function() {

	if ( ! is.null(.self$.db)) {
		DBI::dbDisconnect(.self$.db)
		.db <<- NULL
	}
})
