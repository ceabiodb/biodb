# vi: fdm=marker ts=4 et cc=80 tw=80

# MassSqliteConn {{{1
################################################################################

#' Class for handling a Mass spectrometry database in SQLite format.
#'
#' This is the connector class for a MASS SQLite database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get path to LCMS database example file
#' lcmsdb <- system.file("extdata", "lcmsdb.sqlite", package="biodb")
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('mass.sqlite', url=lcmsdb)
#'
#' # Get an entry
#' e <- conn$getEntry('34.pos.col12.0.78')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbMassdbConn.R
#' @include BiodbEditable.R
#' @include BiodbWritable.R
MassSqliteConn <- methods::setRefClass('MassSqliteConn',
    contains=c("BiodbMassdbConn", 'BiodbWritable', 'BiodbEditable'),
    fields=list(
        .db="ANY"
    ),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) { 

    callSuper(...)

    .self$.db <- NULL
},

# Get entry content from database {{{3
################################################################################

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

            # Loop on all other tables
            for (table in DBI::dbListTables(.self$.db)) {

                # Get data frame
                q <- paste0("select * from `", table, "` where accession='",
                            accession, "';")
                df <- DBI::dbGetQuery(.self$.db, q)

                # Set value
                if (table == 'entries')
                    entry <- c(entry, as.list(df))
                else {
                    cols <- colnames(df)[colnames(df) != 'accession']
                    drop <- ef$get(table)$hasCardMany()
                    entry[[table]]=df[, cols, drop=drop]
                }
            }

            # Set content
            content[[i]] <- entry
        }
    }

    return(content)
},

# Get chromatographic columns {{{3
################################################################################

getChromCol=function(ids=NULL) {
    # Overrides super class' method.

    chrom.cols <- data.frame(id=character(0), title=character(0))

    .self$.initDb()

    if ( ! is.null(.self$.db)) {

        tables <- DBI::dbListTables(.self$.db)

        if ('entries' %in% tables) {

            fields <- DBI::dbListFields(.self$.db, 'entries')
            fields.to.get <- c('chrom.col.id', 'chrom.col.name')

            if (all(fields.to.get %in% fields)) {
                query <- BiodbSqlQuery()
                query$setTable('entries')
                query$setDistinct(TRUE)
                for (field in fields.to.get)
                    query$addField(field=field)

                # Filter on spectra IDs
                if ( ! is.null(ids)) {
                    f <- BiodbSqlField(field='accession')
                    w <- BiodbSqlBinaryOp(op='in',
                                          lexpr=f,
                                          rexpr=BiodbSqlList(ids))
                    query$setWhere(w)
                }

                # Run query
                chrom.cols <- DBI::dbGetQuery(.self$.db, query$toString())
                names(chrom.cols) <- c('id', 'title')
            }
        }
    }

    return(chrom.cols)
},

# Private methods {{{2
################################################################################

# Do write {{{3
################################################################################

.doWrite=function() {

    .self$.initDb()

    if ( ! is.null(.self$.db)) {

        .self$info('Write all new entries into "',
                   .self$getPropValSlot('urls', 'base.url'), '".')

        # Get new entries
        cached.entries <- .self$getAllCacheEntries()
        is.new <- vapply(cached.entries, function(x) x$isNew(), FUN.VALUE=TRUE)
        new.entries <- cached.entries[is.new]

        if (length(new.entries) > 0) {

            # Loop on all new entries and write other fields to separate tables
            i <- 0
            for (entry in new.entries) {

                acc <- entry$getFieldValue('accession')

                # Start transaction
                DBI::dbBegin(.self$.db)

                # Write into main table
                df <- .self$getBiodb()$entriesToDataframe(list(entry),
                                                          only.card.one=TRUE)
                .self$.appendToTable(table='entries', values=df)

                # Loop on all fields
                for (field.name in entry$getFieldNames()) {

                    field <- .self$getBiodb()$getEntryFields()$get(field.name)

                    # Write data frame field
                    if (field$getClass() == 'data.frame') {
                        v <- cbind(accession=acc,
                                   entry$getFieldValue(field.name))
                        .self$.appendToTable(table=field.name, values=v)
                    }

                    # Write multiple values field
                    else if (field$hasCardMany()) {
                        values <- list(accession=acc)
                        values[[field.name]] <- entry$getFieldValue(field.name)
                        DBI::dbWriteTable(conn=.self$.db, name=field.name,
                                          value=as.data.frame(values),
                                          append=TRUE)
                    }
                }

                # Commit transaction
                DBI::dbCommit(.self$.db)

                # Unset "new" flag
                entry$.setAsNew(FALSE)

                # Send progress message
                i <- i + 1
                .self$progressMsg('Writing entries.', index=i,
                                  total=length(new.entries), first=(i == 1))
            }
        }
    }
},

# Init db {{{3
################################################################################

.initDb=function() {

    u <- .self$getPropValSlot('urls', 'base.url')
    if (is.null(.self$.db) && ! is.null(u) && ! is.na(u))
        .self$.db <-  DBI::dbConnect(RSQLite::SQLite(), dbname=u)
},

# Do terminate {{{3
################################################################################

.doTerminate=function() {

    if ( ! is.null(.self$.db)) {
        DBI::dbDisconnect(.self$.db)
        .self$.db <- NULL
    }
},

# Find right M/Z field {{{3
################################################################################

.findMzField=function() {

    mzcol <- NULL

    peak.fields <- DBI::dbListFields(.self$.db, 'peaks')
    for (c in c('peak.mztheo', 'peak.mz', 'peak.mzexp'))
        if (c %in% peak.fields) {
            mzcol <- c
            break
        }

    return(mzcol)
},

# Do get mz values {{{3
################################################################################

# Inherited from BiodbMassdbConn.
.doGetMzValues=function(ms.mode, max.results, precursor, ms.level) {

    mz <- numeric()

    .self$.initDb()

    if ( ! is.null(.self$.db)) {

        # List tables
        tables <- DBI::dbListTables(.self$.db)

        if ('peaks' %in% tables) {

            mzcol <- .self$.findMzField()

            # Build query
            query <- .self$.createMsQuery(mzcol=mzcol, ms.mode=ms.mode,
                                          ms.level=ms.level,
                                          precursor=precursor)
            query$addField(field=mzcol)
            if ( ! is.null(max.results) && ! is.na(max.results))
                query$setLimit(max.results)
            .self$message('debug', paste0('Run query "', query$toString(),
                                          '".'))

            # Run query
            df <- DBI::dbGetQuery(.self$.db, query$toString())
            mz <- df[[1]]
        }
    }

    return(mz)
},

# Create MS query object {{{3
################################################################################

.createMsQuery=function(mzcol, ms.mode=NULL, ms.level=0, precursor=FALSE) {

    query <- BiodbSqlQuery()
    query$setTable('peaks')
    query$setDistinct(TRUE)
    query$setWhere(BiodbSqlLogicalOp(op='and'))

    if (precursor) {
        query$addJoin(table1='msprecmz', field1='accession', table2='peaks',
                      field2='accession')
        expr <- BiodbSqlBinaryOp(lexpr=BiodbSqlField(table='msprecmz',
                                                     field='msprecmz'),
                                 op='=',
                                 rexpr=BiodbSqlField(table='peaks',
                                                     field=mzcol))
        query$getWhere()$addExpr(expr)
    }
    if ( ! is.null(ms.level) && ! is.na(ms.level)
        && (is.numeric(ms.level) || is.integer(ms.level)) && ms.level > 0) {
        query$addJoin(table1='entries', field1='accession',
                      table2='peaks', field2='accession')
        expr <- BiodbSqlBinaryOp(lexpr=BiodbSqlField(table='entries',
                                                     field='ms.level'),
                                 op='=', rexpr=BiodbSqlValue(ms.level))
        query$getWhere()$addExpr(expr)
    }
    if ( ! is.null(ms.mode) && ! is.na(ms.mode) && is.character(ms.mode)) {
        query$addJoin(table1='entries', field1='accession',
                      table2='peaks', field2='accession')
        expr <- BiodbSqlBinaryOp(lexpr=BiodbSqlField(table='entries',
                                                     field='ms.mode'),
                                 op='=', rexpr=BiodbSqlValue(ms.mode))
        query$getWhere()$addExpr(expr)
    }

    return(query)
},

# Do search M/Z range {{{3
################################################################################

.doSearchMzRange=function(mz.min, mz.max, min.rel.int, ms.mode, max.results,
                          precursor, ms.level) {

    ids <- character()

    .self$.initDb()

    if ( ! is.null(.self$.db)) {

        # List tables
        tables <- DBI::dbListTables(.self$.db)

        if ('peaks' %in% tables) {

            mzcol <- .self$.findMzField()

            # Build query
            query <- .self$.createMsQuery(mzcol=mzcol, ms.mode=ms.mode,
                                          ms.level=ms.level,
                                          precursor=precursor)
            query$addField(table='peaks', field='accession')
            mz.range.or=BiodbSqlLogicalOp('or')
            for (i in seq_along(if (is.null(mz.max)) mz.min else mz.max)) {
                and=BiodbSqlLogicalOp('and')
                if ( ! is.null(mz.min) && ! is.na(mz.min[[i]])) {
                    rval <- BiodbSqlValue(as.numeric(mz.min[[i]]))
                    expr <- BiodbSqlBinaryOp(lexpr=BiodbSqlField(table='peaks',
                                                                 field=mzcol),
                                             op='>=', rexpr=rval)
                    and$addExpr(expr)
                }
                if ( ! is.null(mz.max) && ! is.na(mz.max[[i]])) {
                    rval <- BiodbSqlValue(as.numeric(mz.max[[i]]))
                    expr <- BiodbSqlBinaryOp(lexpr=BiodbSqlField(table='peaks',
                                                                 field=mzcol),
                                             op='<=', rexpr=rval)
                    and$addExpr(expr)
                }
                mz.range.or$addExpr(and)
            }
            query$getWhere()$addExpr(mz.range.or)
            if ('peak.relative.intensity' %in% DBI::dbListFields(.self$.db,
                                                                 'peaks')
                && ! is.null(min.rel.int) && ! is.na(min.rel.int)
                && (is.numeric(min.rel.int) || is.integer(min.rel.int))) {
                lval <- BiodbSqlField(table='peaks',
                                      field='peak.relative.intensity')
                rval <- BiodbSqlValue(min.rel.int)
                expr <- BiodbSqlBinaryOp(lexpr=lval, op='>=', rexpr=rval)
                query$getWhere()$addExpr(expr)
            }
            if ( ! is.null(max.results) && ! is.na(max.results))
                query$setLimit(max.results)
            .self$debug('Run query "', query$toString(), '".')

            # Run query
            df <- DBI::dbGetQuery(.self$.db, query$toString())
            ids <- df[[1]]
        }
    }

    return(ids)
},

# Append to table {{{3
################################################################################

.appendToTable=function(table, values) {

    # Append to existing table
    if (table %in% DBI::dbListTables(.self$.db)) {

        # Create new columns
        current.fields <- DBI::dbListFields(.self$.db, name=table)
        new.fields <- colnames(values)[ ! colnames(values) %in% current.fields]
        for (field in new.fields) {
            query <- paste0('alter table ',
                            DBI::dbQuoteIdentifier(DBI::ANSI(), table),
                            ' add ', DBI::dbQuoteIdentifier(DBI::ANSI(), field),
                            ' ', DBI::dbDataType(.self$.db, values[[field]]),
                            ';')
            result <- DBI::dbSendQuery(.self$.db, query)
            DBI::dbClearResult(result)
        }

        # Append to table
        DBI::dbWriteTable(conn=.self$.db, name=table, value=values, append=TRUE)

    # Create table
    } else
        DBI::dbWriteTable(conn=.self$.db, name=table, value=values)
},

# Get entry ids {{{3
################################################################################

.doGetEntryIds=function(max.results=NA_integer_) {

    ids <- integer()

    .self$.initDb()

    if ( ! is.null(.self$.db)) {

        # List tables
        tables <- DBI::dbListTables(.self$.db)

        if ('entries' %in% tables) {

            # Build query
            query <- "select accession from entries"
            if ( ! is.null(max.results) && ! is.na(max.results)
                && (is.numeric(max.results) || is.integer(max.results)))
                query <- paste0(query, ' limit ', as.integer(max.results))

            # Run query
            df <- DBI::dbGetQuery(.self$.db, query)
            ids <- df[[1]]
        }
    }

    return(ids)
}

))
