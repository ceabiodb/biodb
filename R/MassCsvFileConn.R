#' Mass CSV File connector class.
#'
#' This is the connector class for a MASS CSV file database.
#'
#' @seealso Super class \code{\link{CsvFileConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Get path to LCMS database example file
#' lcmsdb <- system.file("extdata",
#'                       "massbank_extract_lcms_2.tsv", package="biodb")
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('mass.csv.file', url=lcmsdb)
#'
#' # Get an entry
#' e <- conn$getEntry('PR010001')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include CsvFileConn.R
#' @export
MassCsvFileConn <- R6::R6Class("MassCsvFileConn",
inherit=CsvFileConn,

public=list(

initialize=function(...) {

    super$initialize(...)

    # Precursors
    private$precursors <- c("[(M+H)]+", "[M+H]+", "[(M+Na)]+", "[M+Na]+",
    "[(M+K)]+", "[M+K]+", "[(M-H)]-", "[M-H]-", "[(M+Cl)]-", "[M+Cl]-")
},

#' @description
#' Gets the list of formulae used to recognize precursors.
#' @return A character vector containing chemical formulae.
getPrecursorFormulae=function() {

    return (private$precursors)
},

#' @description
#' Tests if a formula is a precursor formula.
#' @param formula A chemical formula, as a character value.
#' @return TRUE if the submitted formula is considered a precursor.
isAPrecursorFormula=function(formula) {

    return (formula %in% private$precursors)
},

#' @description
#' Sets the list precursor formulae.
#' @param formulae A character vector containing formulae.
#' @return None.
setPrecursorFormulae=function(formulae) {

    chk::chk_character(formulae)
    private$precursors <- formulae[ ! duplicated(formulae)]
},

#' @description
#' Adds new formulae to the list of formulae used to recognize
#'     precursors.
#' @param formulae A character vector containing formulae.
#' @return None.
addPrecursorFormulae=function(formulae) {

    private$checkParsingHasBegan()

    if ( ! all(formulae %in% private$precursors)) {
        formulae <- formulae[ ! formulae %in% private$precursors]
        private$precursors <- c(private$precursors, formulae)
    }
},

getChromCol=function(ids=NULL) {
    # Overrides super class' method.

    # Extract needed columns
    fields <- c('chrom.col.id', 'chrom.col.name')
    fields <- Filter(function(f) self$hasField(f), fields)
    
    db <- private$select(cols=fields, ids=ids)

    # Remove rows with NA values
    cols <- na.omit(db)

    # Remove duplicates
    cols <- cols[ ! duplicated(cols), , drop=FALSE]

    # Rename columns
    if (ncol(cols) == 0) {
        id <- ttl <- character()
    } else {
        id <- if ('chrom.col.id' %in% names(cols)) cols[['chrom.col.id']] else
            cols[['chrom.col.name']]
        ttl <- if ('chrom.col.name' %in% names(cols))
            cols[['chrom.col.name']] else cols[['chrom.col.id']]
    }
    chrom.cols <- data.frame(id=id, title=ttl)

    return(chrom.cols)
},

getNbPeaks=function(mode=NULL, ids=NULL) {
    # Overrides super class' method.

    # Get peaks
    mzcol <- self$getMatchingMzField()
    peaks <- private$select(cols=mzcol, mode=mode, ids=ids, drop=TRUE)

    return(length(peaks))
}
),

private=list(
    precursors=NULL
,
selectByMode=function(db, mode) {

    # Check mode value
    msModeField <- self$getBiodb()$getEntryFields()$get('ms.mode')
    msModeField$checkValue(mode)
    private$checkFields('ms.mode')

    # Filter on mode
    field <- private$fields[['ms.mode']]
    modesVal <- msModeField$getAllowedValues(mode)
    db <- db[db[[field]] %in% modesVal, , drop=FALSE]

    return(db)
},

selectByCompoundIds=function(db, compound.ids) {

    private$checkFields('compound.id')
    field <- private$fields[['compound.id']]
    db <- db[db[[field]] %in% compound.ids, , drop=FALSE]

    return(db)
},

selectByMzValues=function(db, mz.min, mz.max) {

    mzcol <- self$getMatchingMzField()
    return(private$selectByRange(db=db, field=mzcol, minValue=mz.min,
                                maxValue=mz.max))
},

selectByRelInt=function(db, min.rel.int) {

    if (private$checkFields('peak.relative.intensity', fail=FALSE)) {
        field <- private$fields[['peak.relative.intensity']]
        db <- db[db[[field]] >= min.rel.int, , drop=FALSE]
    }
    else
        db <- db[integer(), , drop=FALSE]

    return(db)
},

selectByPrecursors=function(db) {

    if (private$checkFields('peak.attr', fail=FALSE)) {
        field <- private$fields[['peak.attr']]
        db <- db[db[[field]] %in% private$precursors, , drop=FALSE]
    }
    else
        db <- db[integer(), , drop=FALSE]

    return(db)
},

selectByMsLevel=function(db, level) {

    if (private$checkFields('ms.level', fail=FALSE))
        db <- db[db[[private$fields[['ms.level']]]] == level, , drop=FALSE]
    else
        db <- db[integer(), , drop=FALSE]

    return(db)
},

doSelect=function(db, mode=NULL, compound.ids=NULL, mz.min=NULL, mz.max=NULL, min.rel.int=0, precursor=FALSE, level=0)
{

    # Filtering
    if ( ! is.null(mode) && ! is.na(mode))
        db <- private$selectByMode(db, mode)
    if ( ! is.null(compound.ids))
        db <- private$selectByCompoundIds(db, compound.ids)
    if ( ! is.null(mz.min) || ! is.null(mz.max))
        db <- private$selectByMzValues(db, mz.min, mz.max)
    if (min.rel.int > 0)
        db <- private$selectByRelInt(db, min.rel.int)
    if (precursor)
        db <- private$selectByPrecursors(db)
    if (level > 0)
        db <- private$selectByMsLevel(db, level)

    return(db)
},

doSearchMzRange=function(mz.min, mz.max, min.rel.int, ms.mode, max.results, precursor, ms.level) {
    # Overrides super class' method.
    return(private$select(mz.min=mz.min, mz.max=mz.max, min.rel.int=min.rel.int,
    mode=ms.mode, max.rows=max.results, cols='accession', drop=TRUE, uniq=TRUE,
    sort=TRUE, precursor=precursor, level=ms.level))
},

doGetMzValues=function(ms.mode, max.results, precursor, ms.level) {
    # Overrides super class' method.

    # Get mz values
    mzcol <- self$getMatchingMzField()
    mz <- private$select(cols=mzcol, mode=ms.mode, drop=TRUE, uniq=TRUE,
    sort=TRUE, max.rows=max.results, precursor=precursor, level=ms.level)

    return(mz)
}
))
