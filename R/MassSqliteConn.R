#' Class for handling a Mass spectrometry database in SQLite format.
#'
#' This is the connector class for a MASS SQLite database.
#'
#' @seealso Super classes \code{\link{BiodbMassdbConn}} and
#' \code{\link{SqliteConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get path to LCMS database example file
#' lcmsdb <- system.file("extdata", "massbank_extract.sqlite", package="biodb")
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
#' @import methods
#' @include BiodbMassdbConn.R
#' @include SqliteConn.R
MassSqliteConn <- methods::setRefClass('MassSqliteConn',
    contains=c("SqliteConn", "BiodbMassdbConn"),
    fields=list(),

methods=list(

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
                chrom.cols <- .self$getQuery(query)
                names(chrom.cols) <- c('id', 'title')
            }
        }
    }

    return(chrom.cols)
},

.doGetMzValues=function(ms.mode, max.results, precursor, ms.level) {
    # Inherited from BiodbMassdbConn.

    mz <- numeric()

    .self$.initDb()

    if ( ! is.null(.self$.db)) {

        # Get M/Z field name
        mzfield <- .self$getMatchingMzField()

        if ( ! is.null(mzfield)) {
            mzfield <- .self$.fieldToSqlId(mzfield)

            # Build query
            query <- .self$.createMsQuery(mzfield=mzfield, ms.mode=ms.mode,
                                          ms.level=ms.level,
                                          precursor=precursor)
            query$addField(field=mzfield)
            if (max.results > 0)
                query$setLimit(max.results)
            logDebug('Run query "%s".', query$toString())

            # Run query
            df <- .self$getQuery(query)
            mz <- df[[1]]
        }
    }

    return(mz)
},

.createMsQuery=function(mzfield, ms.mode=NULL, ms.level=0, precursor=FALSE) {

    query <- BiodbSqlQuery()
    query$setTable(mzfield)
    query$setDistinct(TRUE)
    query$setWhere(BiodbSqlLogicalOp(op='and'))

    if (precursor) {
        query$addJoin(table1='msprecmz', field1='accession', table2=mzfield,
                      field2='accession')
        expr <- BiodbSqlBinaryOp(lexpr=BiodbSqlField(table='msprecmz',
                                                     field='msprecmz'),
                                 op='=',
                                 rexpr=BiodbSqlField(table=mzfield,
                                                     field=mzfield))
        query$getWhere()$addExpr(expr)
    }
    if ( ! is.null(ms.level) && ! is.na(ms.level)
        && (is.numeric(ms.level) || is.integer(ms.level)) && ms.level > 0) {
        query$addJoin(table1='entries', field1='accession',
                      table2=mzfield, field2='accession')
        expr <- BiodbSqlBinaryOp(lexpr=BiodbSqlField(table='entries',
                                                     field='ms.level'),
                                 op='=', rexpr=BiodbSqlValue(ms.level))
        query$getWhere()$addExpr(expr)
    }
    if ( ! is.null(ms.mode) && ! is.na(ms.mode) && is.character(ms.mode)) {
        query$addJoin(table1='entries', field1='accession',
                      table2=mzfield, field2='accession')
        expr <- BiodbSqlBinaryOp(lexpr=BiodbSqlField(table='entries',
                                                     field='ms.mode'),
                                 op='=', rexpr=BiodbSqlValue(ms.mode))
        query$getWhere()$addExpr(expr)
    }

    return(query)
},

.doSearchMzRange=function(mz.min, mz.max, min.rel.int, ms.mode, max.results,
                          precursor, ms.level) {

    ids <- character()

    .self$.initDb()

    if ( ! is.null(.self$.db)) {

        # Get M/Z field name
        mzfield <- .self$getMatchingMzField()

        if ( ! is.null(mzfield)) {
            mzfield <- .self$.fieldToSqlId(mzfield)
            mzfield <- DBI::dbQuoteIdentifier(.self$.db, mzfield)

            # Build query
            query <- .self$.createMsQuery(mzfield=mzfield, ms.mode=ms.mode,
                                          ms.level=ms.level,
                                          precursor=precursor)
            query$addField(table=mzfield, field='accession')
            mz.range.or=BiodbSqlLogicalOp('or')
            for (i in seq_along(if (is.null(mz.max)) mz.min else mz.max)) {
                and=BiodbSqlLogicalOp('and')
                if ( ! is.null(mz.min) && ! is.na(mz.min[[i]])) {
                    rval <- BiodbSqlValue(as.numeric(mz.min[[i]]))
                    expr <- BiodbSqlBinaryOp(lexpr=BiodbSqlField(table=mzfield,
                                                                 field=mzfield),
                                             op='>=', rexpr=rval)
                    and$addExpr(expr)
                }
                if ( ! is.null(mz.max) && ! is.na(mz.max[[i]])) {
                    rval <- BiodbSqlValue(as.numeric(mz.max[[i]]))
                    expr <- BiodbSqlBinaryOp(lexpr=BiodbSqlField(table=mzfield,
                                                                 field=mzfield),
                                             op='<=', rexpr=rval)
                    and$addExpr(expr)
                }
                mz.range.or$addExpr(and)
            }
            query$getWhere()$addExpr(mz.range.or)
            if ('peak.relative.intensity' %in% DBI::dbListTables(.self$.db)
                && ! is.null(min.rel.int) && ! is.na(min.rel.int)
                && (is.numeric(min.rel.int) || is.integer(min.rel.int))) {
                query$addJoin(table1=mzfield, field1='accession',
                              table2='peak.relative.intensity',
                              field2='peak.relative.intensity')
                lval <- BiodbSqlField(table='peak.relative.intensity',
                                      field='peak.relative.intensity')
                rval <- BiodbSqlValue(min.rel.int)
                expr <- BiodbSqlBinaryOp(lexpr=lval, op='>=', rexpr=rval)
                query$getWhere()$addExpr(expr)
            }
            if (max.results > 0)
                query$setLimit(max.results)
            logDebug('Run query "%s".', query$toString())

            # Run query
            df <- .self$getQuery(query)
            ids <- df[[1]]
        }
    }

    return(ids)
}

))
