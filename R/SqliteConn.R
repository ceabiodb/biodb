#' SQLite connector class.
#'
#' This is the abstract connector class for all SQLite databases.
#'
#' @seealso Super classes \code{\link{BiodbConn}}, \code{\link{BiodbWritable}},
#' \code{\link{BiodbEditable}}, and sub-classes \code{\link{CompSqliteConn}},
#' and \code{\link{MassSqliteConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
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
#' @import methods
#' @import RSQLite
#' @include BiodbConn.R
#' @include BiodbEditable.R
#' @include BiodbWritable.R
#' @export SqliteConn
#' @exportClass SqliteConn
SqliteConn <- methods::setRefClass("SqliteConn",
    contains=c("BiodbConn", 'BiodbWritable', 'BiodbEditable'),
    fields=list(
        .db="ANY"
    ),

methods=list(

initialize=function(...) {

    callSuper(...)

    .self$.db <- NULL
},

getEntryContentFromDb=function(entry.id) {
    # Overrides super class' method.

    # Initialize contents to return
    content <- rep(list(NULL), length(entry.id))

    .self$.initDb()

    if ( ! is.null(.self$.db)) {

        ef <- .self$getBiodb()$getEntryFields()

        # Loop on all entry IDs
        i <- 0
        for (accession in entry.id) {

            i <- i + 1
            entry <- list()

            # Loop on all tables
            for (tableName in DBI::dbListTables(.self$.db)) {

                # Get data frame
                q <- paste0("select * from ",
                            DBI::dbQuoteIdentifier(.self$.db, tableName),
                            " where accession='",
                            accession, "';")
                df <- DBI::dbGetQuery(.self$.db, q)

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

    entry.fields <- .self$getBiodb()$getEntryFields()

    # Loop on all entry fields
    for (field in entry.fields$getFieldNames()) {
        f <- entry.fields$get(field)
        if ( ! f$isVirtual()) {
            db_id <- if (f$hasCardOne()) field else .self$.fieldToSqlId(field)
            .self$setPropValSlot('parsing.expr', field, db_id)
        }
    }
},

.doWrite=function() {

    .self$.initDb()

    if ( ! is.null(.self$.db)) {

        .self$info('Write all new entries into "',
                   .self$getPropValSlot('urls', 'base.url'), '".')

        # Get new entries
        cached.entries <- .self$getAllVolatileCacheEntries()
        is.new <- vapply(cached.entries, function(x) x$isNew(), FUN.VALUE=TRUE)
        new.entries <- cached.entries[is.new]

        if (length(new.entries) > 0) {

            # Loop on all new entries and write other fields to separate tables
            prg <- Progress$new(biodb=.self$getBiodb(), msg='Writing entries.',
                                total=length(new.entries))
            for (entry in new.entries) {

                acc <- entry$getFieldValue('accession')

                # Start transaction
                DBI::dbBegin(.self$.db)

                # Write into main table
                df <- .self$getBiodb()$entriesToDataframe(list(entry),
                                                          only.card.one=TRUE)
                .self$.appendToTable(tableName='entries', values=df)

                # Loop on all fields
                for (field.name in entry$getFieldNames()) {

                    field <- .self$getBiodb()$getEntryFields()$get(field.name)

                    # Write data frame field
                    if (field$getClass() == 'data.frame') {
                        v <- cbind(accession=acc,
                                   entry$getFieldValue(field.name))
                        .self$.appendToTable(tableName=.self$.fieldToSqlId(field.name), values=v)
                    }

                    # Write multiple values field
                    else if (field$hasCardMany()) {
                        values <- list(accession=acc)
                        values[[.self$.fieldToSqlId(field.name)]] <- entry$getFieldValue(field.name)
                        DBI::dbWriteTable(conn=.self$.db, name=.self$.fieldToSqlId(field.name),
                                          value=as.data.frame(values),
                                          append=TRUE)
                    }
                }

                # Commit transaction
                DBI::dbCommit(.self$.db)

                # Unset "new" flag
                entry$.setAsNew(FALSE)

                # Send progress message
                prg$increment()
            }
        }
    }
},

.initDb=function() {

    u <- .self$getPropValSlot('urls', 'base.url')
    if (is.null(.self$.db) && ! is.null(u) && ! is.na(u))
        .self$.db <-  DBI::dbConnect(RSQLite::SQLite(), dbname=u)
},

isSearchableByField=function(field) {
    # Overrides super class' method.

    v <- callSuper(field)
    v <- v && .self$hasField(field)
    
    return(v)
},

hasField=function(field) {
    
    b <- FALSE
    .self$.initDb()
    
    ef <- .self$getBiodb()$getEntryFields()
    
    # Check that a column is defined inside main table
    if (ef$get(field)$hasCardOne())
        b <- field %in% DBI::dbListFields(.self$.db, 'entries')

    # Check that a table is defined for this field
    else
        b <- DBI::dbExistsTable(.self$.db, .self$.fieldToSqlId(field))
    
    return(b)
},

getQuery=function(query) {
    query_str <- query$toString()
    logDebug('Running query "%s".', query_str)
    return(DBI::dbGetQuery(.self$.db, query_str))
},

.doTerminate=function() {

    if ( ! is.null(.self$.db)) {
        DBI::dbDisconnect(.self$.db)
        .self$.db <- NULL
    }
},

.appendToTable=function(tableName, values) {

    # Append to existing table
    if (tableName %in% DBI::dbListTables(.self$.db)) {

        # Create new columns
        current.fields <- DBI::dbListFields(.self$.db, name=tableName)
        new.fields <- colnames(values)[ ! colnames(values) %in% current.fields]
        for (field in new.fields) {
            query <- paste0('alter table ',
                            DBI::dbQuoteIdentifier(.self$.db, tableName),
                            ' add ', DBI::dbQuoteIdentifier(.self$.db, field),
                            ' ', DBI::dbDataType(.self$.db, values[[field]]),
                            ';')
            result <- DBI::dbSendQuery(.self$.db, query)
            DBI::dbClearResult(result)
        }

        # Append to table
        DBI::dbWriteTable(conn=.self$.db, name=tableName, value=values,
                          append=TRUE)

    # Create table
    } else
        DBI::dbWriteTable(conn=.self$.db, name=tableName, value=values)
},

.doGetEntryIds=function(max.results=0) {

    ids <- integer()

    .self$.initDb()

    if ( ! is.null(.self$.db)) {

        # List tables
        tables <- DBI::dbListTables(.self$.db)

        if ('entries' %in% tables) {

            # Build query
            query <- "select accession from entries"
            if (max.results > 0)
                query <- paste0(query, ' limit ', as.integer(max.results))

            # Run query
            df <- DBI::dbGetQuery(.self$.db, query)
            ids <- df[[1]]
        }
    }

    return(ids)
},

.fieldToSqlId=function(f) {

    f <- gsub('\\.', '_', f)

    return(f)
}

))
