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

	if ( ! is.null(.self$.db)) {

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
	}

	return(ids)
})

# Get entry content {{{1
################################################################

MassSqliteConn$methods( getEntryContent = function(entry.id) {
	
	# Initialize contents to return
	content = rep(list(NULL), length(entry.id))

	.self$.init.db()

	if ( ! is.null(.self$.db)) {

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
	}

	return(content)
})

# Get chromatographic columns {{{1
################################################################

MassSqliteConn$methods( getChromCol = function(ids = NULL) {

	chrom.cols <- data.frame(id = character(0), title = character(0))

	.self$.init.db()

	if ( ! is.null(.self$.db)) {

		tables = DBI::dbListTables(.self$.db)

		if ('entries' %in% tables) {

			fields = DBI::dbListFields(.self$.db, 'entries')
			fields.to.get = c('chrom.col.id', 'chrom.col.name')

			if (all(fields.to.get %in% fields)) {
				query = BiodbSqlQuery()
				query$setTable('entries')
				query$setDistinct(TRUE)
				for (field in fields.to.get)
					query$addField(field = field)

				# Filter on spectra IDs
				if ( ! is.null(ids))
					query$setWhere(BiodbSqlBinaryOp(op = 'in', lexpr = BiodbSqlField(field = 'accession'), rexpr = BiodbSqlList(ids)))

				# Run query
				chrom.cols = DBI::dbGetQuery(.self$.db, query$toString())
				names(chrom.cols) = c('id', 'title')
			}
		}
	}

	return(chrom.cols)
})

# Private methods {{{1
################################################################

# Do write {{{2
################################################################

MassSqliteConn$methods( .doWrite = function() {

	.self$.init.db()

	if ( ! is.null(.self$.db)) {

		.self$message('info', paste0('Write all new entries into "', .self$getUrl('base.url'), '".'))

		# Get new entries
		cached.entries = .self$getAllCacheEntries()
		new.entries = cached.entries[vapply(cached.entries, function(x) x$isNew(), FUN.VALUE = TRUE)]

		if (length(new.entries) > 0) {

			# Loop on all new entries and write other fields to separate tables
			i = 0
			for (entry in new.entries) {

				# Start transaction
				DBI::dbBegin(.self$.db)

				# Write into main table
				df = .self$getBiodb()$entriesToDataframe(list(entry), only.card.one = TRUE)
				DBI::dbWriteTable(conn = .self$.db, name = 'entries', value = df, append = TRUE)

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

				# Commit transaction
				DBI::dbCommit(.self$.db)

				# Unset "new" flag
				entry$.setAsNew(FALSE)

				# Send progress message
				i = i + 1
				lapply(.self$getBiodb()$getObservers(), function(x) x$progress(type = 'info', msg = 'Writing entries.', index = i, total = length(new.entries), first = (i == 1)))
			}
		}
	}
})

# Init db {{{2
################################################################

MassSqliteConn$methods( .init.db = function() {

	if (is.null(.self$.db) && ! is.null(.self$getUrl('base.url')) && ! is.na(.self$getUrl('base.url')))
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

# Find right M/Z field {{{2
################################################################

MassSqliteConn$methods( .findMzField = function() {

	mzcol = NULL

	peak.fields = DBI::dbListFields(.self$.db, 'peaks')
	for (c in c('peak.mztheo', 'peak.mz', 'peak.mzexp'))
		if (c %in% peak.fields) {
			mzcol = c
			break
		}

	return(mzcol)
})

# Do get mz values {{{2
################################################################

# Inherited from MassdbConn.
MassSqliteConn$methods( .doGetMzValues = function(ms.mode, max.results, precursor, ms.level) {

	mz = numeric()

	.self$.init.db()

	if ( ! is.null(.self$.db)) {

		# List tables
		tables = DBI::dbListTables(.self$.db)

		if ('peaks' %in% tables) {

			mzcol = .self$.findMzField()

			# Build query
			query = .self$.createMsQuery(mzcol = mzcol, ms.mode = ms.mode, ms.level = ms.level, precursor = precursor)
			query$addField(field = mzcol)
			if ( ! is.null(max.results) && ! is.na(max.results))
				query$setLimit(max.results)
			.self$message('debug', paste0('Run query "', query$toString(), '".'))

			# Run query
			df = DBI::dbGetQuery(.self$.db, query$toString())
			mz = df[[1]]
		}
	}

	return(mz)
})

# Create MS query object {{{2
################################################################

MassSqliteConn$methods( .createMsQuery = function(mzcol, ms.mode = NULL, ms.level = 0, precursor =  FALSE) {

	query = BiodbSqlQuery()
	query$setTable('peaks')
	query$setDistinct(TRUE)
	query$setWhere(BiodbSqlLogicalOp(op = 'and'))

	if (precursor) {
		query$addJoin(table1 = 'msprecmz', field1 = 'accession', table2 = 'peaks', field2 = 'accession')
		query$getWhere()$addExpr(BiodbSqlBinaryOp(lexpr = BiodbSqlField(table = 'msprecmz', field = 'msprecmz'), op = '=', rexpr = BiodbSqlField(table = 'peaks', field = mzcol)))
	}
	if ( ! is.null(ms.level) && ! is.na(ms.level) && (is.numeric(ms.level) || is.integer(ms.level)) && ms.level > 0) {
		query$addJoin(table1 = 'entries', field1 = 'accession', table2 = 'peaks', field2 = 'accession')
		query$getWhere()$addExpr(BiodbSqlBinaryOp(lexpr = BiodbSqlField(table = 'entries', field = 'ms.level'), op = '=', rexpr = BiodbSqlValue(ms.level)))
	}
	if ( ! is.null(ms.mode) && ! is.na(ms.mode) && is.character(ms.mode)) {
		query$addJoin(table1 = 'entries', field1 = 'accession', table2 = 'peaks', field2 = 'accession')
		query$getWhere()$addExpr(BiodbSqlBinaryOp(lexpr = BiodbSqlField(table = 'entries', field = 'ms.mode'), op = '=', rexpr = BiodbSqlValue(ms.mode)))
	}

	return(query)
})

# Do search M/Z range {{{2
################################################################

MassSqliteConn$methods( .doSearchMzRange = function(mz.min, mz.max, min.rel.int, ms.mode, max.results, precursor, ms.level) {

	ids = character()

	.self$.init.db()

	if ( ! is.null(.self$.db)) {

		# List tables
		tables = DBI::dbListTables(.self$.db)

		if ('peaks' %in% tables) {

			mzcol = .self$.findMzField()

			# Build query
			query = .self$.createMsQuery(mzcol = mzcol, ms.mode = ms.mode, ms.level = ms.level, precursor = precursor)
			query$addField(table = 'peaks', field = 'accession')
			mz.range.or = BiodbSqlLogicalOp('or')
			for (i in seq_along(if (is.null(mz.max)) mz.min else mz.max)) {
				and = BiodbSqlLogicalOp('and')
				if ( ! is.null(mz.min) && ! is.na(mz.min[[i]]))
					and$addExpr(BiodbSqlBinaryOp(lexpr = BiodbSqlField(table = 'peaks', field = mzcol), op = '>=', rexpr = BiodbSqlValue(as.numeric(mz.min[[i]]))))
				if ( ! is.null(mz.max) && ! is.na(mz.max[[i]]))
					and$addExpr(BiodbSqlBinaryOp(lexpr = BiodbSqlField(table = 'peaks', field = mzcol), op = '<=', rexpr = BiodbSqlValue(as.numeric(mz.max[[i]]))))
				mz.range.or$addExpr(and)
			}
			query$getWhere()$addExpr(mz.range.or)
			if ( 'peak.relative.intensity' %in% DBI::dbListFields(.self$.db, 'peaks') && ! is.null(min.rel.int) && ! is.na(min.rel.int) && (is.numeric(min.rel.int) || is.integer(min.rel.int)))
				query$getWhere()$addExpr(BiodbSqlBinaryOp(lexpr = BiodbSqlField(table = 'peaks', field = 'peak.relative.intensity'), op = '>=', rexpr = BiodbSqlValue(min.rel.int)))
			if ( ! is.null(max.results) && ! is.na(max.results))
				query$setLimit(max.results)
			.self$message('debug', paste0('Run query "', query$toString(), '".'))

			# Run query
			df = DBI::dbGetQuery(.self$.db, query$toString())
			ids = df[[1]]
		}
	}

	return(ids)
})
