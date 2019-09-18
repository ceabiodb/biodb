# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbCompounddbConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' An interface for all compound databases.
#'
#' This interface is inherited by all compound databases. It declares
#' method headers specific to compound databases.
#'
#' @seealso \code{\link{BiodbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get the connector of a compound database
#' conn <- mybiodb$getFactory()$createConn('chebi')
#'
#' # Search for compounds
#' conn$searchCompound(name='prion protein', max.results=10)
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbConn.R
#' @export BiodbCompounddbConn
#' @exportClass BiodbCompounddbConn
BiodbCompounddbConn <- methods::setRefClass("BiodbCompounddbConn",
    contains="BiodbConn",

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbCompounddbConn')
},

# Search compound {{{3
################################################################################

searchCompound=function(name=NULL, mass=NULL, mass.field=NULL,
                        mass.tol=0.01, mass.tol.unit='plain',
                        max.results=NA_integer_) {
    "Search for compounds by name and/or by mass. At least one of name or mass
    must be set.
    \nname: The name of a compound to search for.
    \nmass: The searched mass.
    \nmass.field: For searching by mass, you must indicate a mass field to use
    ('monoisotopic.mass', 'molecular.mass', 'average.mass' or 'nominal.mass').
    \nmass.tol: The tolerance value on the molecular mass.
    \nmass.tol.unit: The type of mass tolerance. Either 'plain' or 'ppm'.
    \nmax.results: The maximum number of matches to return.
    \nReturned value: A character vector of entry IDs."

    .self$.checkMassField(mass=mass, mass.field=mass.field)

    ids <- NULL

    .self$caution('Database ', .self$getDbClass(),
                  ' is not searchable by mass.')

    return(ids)
},

# Annotate M/Z values {{{3
################################################################################

annotateMzValues=function(x, mz.tol, ms.mode, mz.tol.unit=c('plain', 'ppm'),
                          mass.field='monoisotopic.mass',
                          max.results=3, mz.col='mz',
                          fields=NULL, prefix=NULL, insert.input.values=TRUE) {
    "Annotate 
    \nx: Either a data frame or a numeric vector.
    \nfields: A character vector containing the additional entry fields you
    would like to get for each matched entry. Each field will be output in a
    different column.
    \ninsert.input.values: Insert input values at the beginning of the
    result data frame.
    \nmass.field: The mass field to use for matching M/Z values. One of:
    'monoisotopic.mass', 'molecular.mass', 'average.mass', 'nominal.mass'.
    \nmax.results: If set, it is used to limit the number of matches found for
    each M/Z value. To get all the matches, set this parameter to NA_integer_.
    Default value is 3.
    \nms.mode: The MS mode. Set it to either 'neg' or 'pos'.
    \nmz.col: The name of the column where to find M/Z values in case x is a
    data frame.
    \nmz.tol.unit: The type of the M/Z tolerance. Set it to either to 'ppm' or
    \nprefix: A prefix that will be inserted before the name of each added
    column in the output. By default it will be set to the name of the database
    followed by a dot.
    'plain'.
    \nReturned value: A data frame containing the input values, and annotation
    columns appended at the end. The first annotation column contains the IDs
    of the matched entries. The following columns contain the fields you have
    requested through the `fields` parameter.
    "

    if (is.null(x))
        return(NULL)

    ret <- data.frame(stringsAsFactors=FALSE)
    newCols <- character()
    mz.tol.unit <- match.arg(mz.tol.unit)
    ef <- .self$getBiodb()$getEntryFields()

    # Convert x to data frame
    if ( ! is.data.frame(x))
        x <- data.frame(mz = x)

    # Check mass field
    mass.fields <- ef$getFieldNames('mass')
    .self$.assertIn(mass.field, mass.fields)

    # Check that we find the M/Z column
    if (nrow(x) > 0 && ! mz.col %in% names(x))
        .self$error('No column named "', mz.col,
                    '" was found inside data frame.')

    # Set M/Z col in output data frame
    if (mz.col %in% names(x))
        ret[[mz.col]] <- numeric()

    # Set output fields
    if ( ! is.null(fields))
        ef$checkIsDefined(fields)
    if (is.null(fields))
        fields <- .self$getEntryIdField()

    # Set prefix
    if (is.null(prefix))
        prefix <- paste0(.self$getId(), '.')

    # Get proton mass
    pm <- .self$getBiodb()$getConfig()$get('proton.mass')

    # Loop on all masses
    for (i in seq_len(nrow(x))) {

        # Send progress message
        .self$progressMsg(msg='Annotating M/Z values.', index=i,
                          total=nrow(x), first=(i == 1))

        # Compute mass
        m <- x[i, mz.col] + pm * (if (ms.mode == 'neg') +1.0 else -1.0)

        # Search for compounds matching this mass
        ids <- .self$searchCompound(mass=m, mass.field=mass.field,
            mass.tol=mz.tol, mass.tol.unit=mz.tol.unit, max.results=max.results)

        # Get entries
        entries <- .self$getEntry(ids, drop=FALSE)

        # Convert entries to data frame
        df <- .self$getBiodb()$entriesToDataframe(entries, fields=fields)

        # Add prefix
        if ( ! is.null(df) && ncol(df) > 0 && ! is.na(prefix)
            && nchar(prefix) > 0) {
            fct <- function(x) substr(x, 1, nchar(prefix)) != prefix
            noprefix <- vapply(colnames(df), fct, FUN.VALUE=TRUE)
            colnames(df)[noprefix] <- paste0(prefix,
                                             colnames(df)[noprefix])
    }

        # Register new columns
        if ( ! is.null(df)) {
            c <- colnames(df)[ ! colnames(df) %in% newCols]
            newCols <- c(newCols, c)
        }

        # Insert input values
        if (insert.input.values)
            df <- if (is.null(df) || nrow(df) == 0) x[i, , drop=FALSE]
                else cbind(x[i, , drop=FALSE], df, row.names=NULL,
                           stringsAsFactors=FALSE)

        # Append local data frame to main data frame
        ret <- plyr::rbind.fill(ret, df)
    }

    # Sort new columns
    if ( ! is.null(ret)) {
        isAnInputCol <- ! colnames(ret) %in% newCols
        inputCols <- colnames(ret)[isAnInputCol]
        ret <- ret[, c(inputCols, sort(newCols)), drop=FALSE]
    }

    return(ret)
},

# Private methods {{{2
################################################################################

# Check mass field {{{3
################################################################################

.checkMassField=function(mass, mass.field) {

    if ( ! is.null(mass)) {
        .self$.assertIs(mass, c('integer', 'numeric'))
        .self$.assertNotNull(mass.field)
        .self$.assertIs(mass.field, 'character')
        ef <- .self$getBiodb()$getEntryFields()
        mass.fields <- ef$getFieldNames(type='mass')
        .self$.assertIn(mass.field, mass.fields)
    }
}

))
