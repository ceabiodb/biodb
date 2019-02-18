# vi: fdm=marker


#' Class for handling a Mass spectrometry database in SQLite format.
#'
#' @include biodb-common.R
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

# Writable methods {{{2
################################################################

# Do write {{{3
################################################################

MassSqliteConn$methods( .doWrite = function() {

	.self$.init.db()

	# Loop on all new entries
	{

		# Check if some fields are missing in the SQL table
		# If yes, add them

		# Insert entry

		# Insert mass peaks
	}
})

# Init db {{{2
################################################################

MassSqliteConn$methods( .init.db = function() {

	if (is.null(.self$.db)) {
		.db <<-  DBI::dbConnect(RSQLite::SQLite(), dbname = .self$getUrl('base.url'))

		# Check if tables exist
		tables = DBI::dbListTables(.self$.db)

		print('-------------------------------- MassSqliteConn::.init.db 10')
		print(class(tables))
		print(tables)
	}
})
