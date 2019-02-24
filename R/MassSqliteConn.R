# vi: fdm=marker

# TODO Create BiodbSqlConn class.
# TODO Rename MassdbConn into BiodbMassConn.

# Class declaration {{{1
################################################################

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

# Get entry ids {{{1
################################################################

MassSqliteConn$methods( getEntryIds = function(max.results = NA_integer_) {

	ids = integer()

	.self$.init.db()

	# List tables
	tables = DBI::dbListTables(.self$.db)

	if ('entries' %in% tables) {

		# Build query
		query = "select accession from entries"
		if ( ! is.null(max.results) && ! is.na(max.results) && (is.numeric(max.results) || is.integer(max.results)))
			query = paste0(query, ' limit ', as.integer(max.results))

		# Run query
		df = DBI::dbGetQuery(.self$.db, query)
		ids = df[[1]]
	}

	return(ids)
})

# Get entry content {{{1
################################################################

MassSqliteConn$methods( getEntryContent = function(entry.id) {
	
	# Initialize contents to return
	content = list()

	.self$.init.db()

	# Loop on all entry IDs
	i = 0
	for (accession in entry.id) {

		i = i + 1
		entry = list()

		# Loop on all other tables
		for (table in DBI::dbListTables(.self$.db)) {

			# Get data frame
			df = DBI::dbGetQuery(.self$.db, paste0("select * from `", table, "` where accession = '", accession, "';"))

			# Set value
			if (table == 'entries')
				entry = c(entry, as.list(df))
			else
				entry[[table]] = df[, colnames(df)[colnames(df) != 'accession'], drop = .self$getBiodb()$getEntryFields()$get(table)$hasCardMany()]
		}

		# Set content
		content[[i]] = entry
	}

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

# Do get mz values {{{2
################################################################

# Inherited from MassdbConn.
MassSqliteConn$methods( .doGetMzValues = function(ms.mode, max.results, precursor, ms.level) {

	mz = numeric()

	.self$.init.db()

	# List tables
	tables = DBI::dbListTables(.self$.db)

	if ('peaks' %in% tables) {

		# Found M/Z right column
		mzcol = NULL
		peak.fields = DBI::dbListFields(.self$.db, 'peaks')
		for (c in c('peak.mztheo', 'peak.mz', 'peak.mzexp'))
			if (c %in% peak.fields) {
				mzcol = c
				break
			}

		# Build query
		join = character()
		where = character()
		if (precursor) {
			join = c(join, 'msprecmz')
			where = c(where, paste0('msprecmz.msprecmz = `', mzcol, '`'))
		}
		if ( ! is.null(ms.level) && ! is.na(ms.level) && (is.numeric(ms.level) || is.integer(ms.level)) && ms.level > 0) {
			join = c(join, 'entries')
			where = c(where, paste0('entries.`ms.level` = ', ms.level))
		}
		if ( ! is.null(ms.mode) && ! is.na(ms.mode) && is.character(ms.mode)) {
			join = c(join, 'entries')
			where = c(where, paste0('entries.`ms.mode` = "', ms.mode, '"'))
		}
		query = paste0("select distinct `", mzcol, "` from peaks")
		if (length(join) > 0)
			query = paste(query, paste(vapply(join[ ! duplicated(join)], function(table) paste0('join ', table, ' on ', table, '.accession = peaks.accession'), FUN.VALUE = ''), collapse = ' '))
		if (length(where) > 0)
			query = paste(query, paste0('where ', paste(where, collapse = ' and ')))
		if ( ! is.null(max.results) && ! is.na(max.results) && (is.numeric(max.results) || is.integer(max.results)))
			query = paste0(query, ' limit ', as.integer(max.results))
		query = paste0(query, ';')
		.self$message('debug', paste0('Send query "', query, '".'))

		# Run query
		df = DBI::dbGetQuery(.self$.db, query)
		mz = df[[1]]
	}

	return(mz)
})
