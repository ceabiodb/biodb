#' SQLite connector class.
#'
#' This is the abstract connector class for all SQLite databases.
#'
#' @seealso Super class \code{\link{BiodbConn}} and sub-classes
#' \code{\link{CompSqliteConn}}, and \code{\link{MassSqliteConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Get a connector that inherits from SqliteConn:
#' chebi_file <- system.file("extdata", "chebi_extract.sqlite", package="biodb")
#' conn <- mybiodb$getFactory()$createConn('comp.sqlite', url=chebi_file)
#'
#' # Get an entry
#' e <- conn$getEntry('1018')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import R6
#' @import RSQLite
#' @include BiodbConn.R
#' @export
SqliteConn <- R6::R6Class("SqliteConn",
inherit=BiodbConn,

public=list(

initialize=function(...) {

    super$initialize(...)

    private$db <- NULL
},

getEntryContentFromDb=function(entry.id) {
    # Overrides super class' method.

    # Initialize contents to return
    content <- rep(list(NULL), length(entry.id))

    private$initDb()

    if ( ! is.null(private$db)) {

        ef <- self$getBiodb()$getEntryFields()

        # Loop on all entry IDs
        i <- 0
        for (accession in entry.id) {

            i <- i + 1
            entry <- list()

            # Loop on all tables
            for (tableName in DBI::dbListTables(private$db)) {

                # Get data frame
                q <- paste0("select * from ",
                    DBI::dbQuoteIdentifier(private$db, tableName), " where
                    accession='", accession, "';")
                df <- DBI::dbGetQuery(private$db, q)

                # Set value
                if (tableName == 'entries')
                    entry <- c(entry, as.list(df))
                else {
                    cols <- colnames(df)[colnames(df) != 'accession']
                    dropFlag <- ef$get(tableName)$hasCardMany()
                    entry[[tableName]] <- df[, cols, drop=dropFlag]
                }
            }

            # Set content
            content[[i]] <- entry
        }
    }

    return(content)
},

defineParsingExpressions=function() {
    # Overrides super class' method.

    entry.fields <- self$getBiodb()$getEntryFields()

    # Loop on all entry fields
    for (field in entry.fields$getFieldNames()) {
        f <- entry.fields$get(field)
        if ( ! f$isVirtual()) {
            db_id <- if (f$hasCardOne()) field else private$fieldToSqlId(field)
            self$setPropValSlot('parsing.expr', field, db_id)
        }
    }
},

isSearchableByField=function(field) {
    # Overrides super class' method.

    v <- super$isSearchableByField(field)
    v <- v && self$hasField(field)
    
    return(v)
},

hasField=function(field) {
    
    b <- FALSE
    private$initDb()
    
    ef <- self$getBiodb()$getEntryFields()
    
    # Check that a column is defined inside main table
    if (ef$get(field)$hasCardOne())
        b <- field %in% DBI::dbListFields(private$db, 'entries')

    # Check that a table is defined for this field
    else
        b <- DBI::dbExistsTable(private$db, private$fieldToSqlId(field))
    
    return(b)
},

getQuery=function(query) {
    query_str <- query$toString()
    logDebug('Running query "%s".', query_str)
    return(DBI::dbGetQuery(private$db, query_str))
}
),

private=list(
    db=NULL
,
doWrite=function() {

    private$initDb()

    if ( ! is.null(private$db)) {

        logInfo('Write all new entries into "%s".',
            self$getPropValSlot('urls', 'base.url'))

        # Get new entries
        cached.entries <- self$getAllVolatileCacheEntries()
        is.new <- vapply(cached.entries, function(x) x$isNew(), FUN.VALUE=TRUE)
        new.entries <- cached.entries[is.new]

        if (length(new.entries) > 0) {

            # Loop on all new entries and write other fields to separate tables
            prg <- Progress$new(biodb=self$getBiodb(), msg='Writing entries.',
                total=length(new.entries))
            for (entry in new.entries) {

                acc <- entry$getFieldValue('accession')

                # Start transaction
                DBI::dbBegin(private$db)

                # Write into main table
                df <- self$getBiodb()$entriesToDataframe(list(entry),
                    only.card.one=TRUE)
                private$appendToTable(tableName='entries', values=df)

                # Loop on all fields
                for (field.name in entry$getFieldNames()) {

                    field <- self$getBiodb()$getEntryFields()$get(field.name)

                    # Write data frame field
                    if (field$getClass() == 'data.frame') {
                        v <- cbind(accession=acc,
                            entry$getFieldValue(field.name))
                        private$appendToTable(
                            ableName=private$fieldToSqlId(field.name), values=v)
                    }

                    # Write multiple values field
                    else if (field$hasCardMany()) {
                        values <- list(accession=acc)
                        values[[private$fieldToSqlId(field.name)]] <-
                            entry$getFieldValue(field.name)
                        DBI::dbWriteTable(conn=private$db,
                            name=private$fieldToSqlId(field.name),
                            value=as.data.frame(values), append=TRUE)
                    }
                }

                # Commit transaction
                DBI::dbCommit(private$db)

                # Unset "new" flag
                entry$setAsNew(FALSE)

                # Send progress message
                prg$increment()
            }
        }
    }
},

initDb=function() {

    u <- self$getPropValSlot('urls', 'base.url')
    if (is.null(private$db) && ! is.null(u) && ! is.na(u))
        private$db <-  DBI::dbConnect(RSQLite::SQLite(), dbname=u)
},

terminate=function() {

    if ( ! is.null(private$db)) {
        DBI::dbDisconnect(private$db)
        private$db <- NULL
    }

    super$terminate()
},

appendToTable=function(tableName, values) {

    # Append to existing table
    if (tableName %in% DBI::dbListTables(private$db)) {

        # Create new columns
        current.fields <- DBI::dbListFields(private$db, name=tableName)
        new.fields <- colnames(values)[ ! colnames(values) %in% current.fields]
        for (field in new.fields) {
            query <- paste0('alter table ',
                DBI::dbQuoteIdentifier(private$db, tableName), ' add ',
                DBI::dbQuoteIdentifier(private$db, field), ' ',
                DBI::dbDataType(private$db, values[[field]]), ';')
            result <- DBI::dbSendQuery(private$db, query)
            DBI::dbClearResult(result)
        }

        # Append to table
        DBI::dbWriteTable(conn=private$db, name=tableName, value=values,
            append=TRUE)

    # Create table
    } else
        DBI::dbWriteTable(conn=private$db, name=tableName, value=values)
},

doGetEntryIds=function(max.results=0) {

    ids <- integer()

    private$initDb()

    if ( ! is.null(private$db)) {

        # List tables
        tables <- DBI::dbListTables(private$db)

        if ('entries' %in% tables) {

            # Build query
            query <- "select accession from entries"
            if (max.results > 0)
                query <- paste0(query, ' limit ', as.integer(max.results))

            # Run query
            df <- DBI::dbGetQuery(private$db, query)
            ids <- df[[1]]
        }
    }

    return(ids)
},

fieldToSqlId=function(f) {

    f <- gsub('\\.', '_', f)

    return(f)
}
))
