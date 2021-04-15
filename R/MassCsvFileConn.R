#' Mass CSV File connector class.
#'
#' This is the connector class for a MASS CSV file database.
#'
#' @seealso Super class \code{\link{CsvFileConn}} and interfaces
#' \code{\link{BiodbMassdbConn}}, \code{\link{BiodbWritable}} and
#' \code{\link{BiodbEditable}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
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
#' @include BiodbMassdbConn.R
#' @export MassCsvFileConn
#' @exportClass MassCsvFileConn
MassCsvFileConn <- methods::setRefClass("MassCsvFileConn",
    contains=c("CsvFileConn", "BiodbMassdbConn"),
    fields=list(
        .precursors="character"
        ),

methods=list(

initialize=function(...) {

    callSuper(...)

    # Precursors
    .self$.precursors <- c("[(M+H)]+", "[M+H]+", "[(M+Na)]+", "[M+Na]+",
                           "[(M+K)]+", "[M+K]+", "[(M-H)]-", "[M-H]-",
                           "[(M+Cl)]-", "[M+Cl]-")
},

getPrecursorFormulae=function() {
    ":\n\nGets the list of formulae used to recognize precursors.
    \nReturned value: A character vector containing chemical formulae.
    "

    return (.self$.precursors)
},

isAPrecursorFormula=function(formula) {
    ":\n\nTests if a formula is a precursor formula.
    \nformula: A chemical formula, as a character value.
    \nReturned value: TRUE if the submitted formula is considered a precursor.
    "

    return (formula %in% .self$.precursors)
},

setPrecursorFormulae=function(formulae) {
    ":\n\nSets the list precursor formulae.
    \nformulae: A character vector containing formulae.
    \nReturned value: None.
    "

    chk::chk_character(formulae)
    .self$.precursors <- formulae[ ! duplicated(formulae)]
},

addPrecursorFormulae=function(formulae) {
    ":\n\nAdds new formulae to the list of formulae used to recognize precursors.
    \nformulae: A character vector containing formulae.
    \nReturned value: None.
    "

    .self$.checkParsingHasBegan()

    if ( ! all(formulae %in% .self$.precursors)) {
        formulae <- formulae[ ! formulae %in% .self$.precursors]
        .self$.precursors <- c(.self$.precursors, formulae)
    }
},

getChromCol=function(ids=NULL) {
    # Overrides super class' method.

    # Extract needed columns
    db <- .self$.select(cols='chrom.col.name', ids=ids)

    # Get column names
    cols <- db[[.self$.fields[['chrom.col.name']]]]

    # Remove NA values
    cols <- cols[ ! is.na(cols)]

    # Remove duplicates
    cols <- cols[ ! duplicated(cols)]

    # Make data frame
    if (is.null(cols))
        chrom.cols <- data.frame(a=character(0), b=character(0))
    else
        chrom.cols <- data.frame(cols, cols, stringsAsFactors=FALSE)
    names(chrom.cols) <- c('id', 'title')

    return(chrom.cols)
},

getNbPeaks=function(mode=NULL, ids=NULL) {
    # Overrides super class' method.

    # Get peaks
    mzcol <- .self$getMatchingMzField()
    peaks <- .self$.select(cols=mzcol, mode=mode, ids=ids, drop=TRUE)

    return(length(peaks))
},

.selectByMode=function(db, mode) {

    # Check mode value
    msModeField <- .self$getBiodb()$getEntryFields()$get('ms.mode')
    msModeField$checkValue(mode)
    .self$.checkFields('ms.mode')

    # Filter on mode
    field <- .self$.fields[['ms.mode']]
    modesVal <- msModeField$getAllowedValues(mode)
    db <- db[db[[field]] %in% modesVal, , drop=FALSE]

    return(db)
},

.selectByCompoundIds=function(db, compound.ids) {

    .self$.checkFields('compound.id')
    field <- .self$.fields[['compound.id']]
    db <- db[db[[field]] %in% compound.ids, , drop=FALSE]

    return(db)
},

.selectByMzValues=function(db, mz.min, mz.max) {

    mzcol <- .self$getMatchingMzField()
    return(.self$.selectByRange(db=db, field=mzcol, minValue=mz.min,
                                maxValue=mz.max))
},

.selectByRelInt=function(db, min.rel.int) {

    if (.self$.checkFields('peak.relative.intensity', fail=FALSE)) {
        field <- .self$.fields[['peak.relative.intensity']]
        db <- db[db[[field]] >= min.rel.int, , drop=FALSE]
    }
    else
        db <- db[integer(), , drop=FALSE]

    return(db)
},

.selectByPrecursors=function(db) {

    if (.self$.checkFields('peak.attr', fail=FALSE)) {
        field <- .self$.fields[['peak.attr']]
        db <- db[db[[field]] %in% .self$.precursors, , drop=FALSE]
    }
    else
        db <- db[integer(), , drop=FALSE]

    return(db)
},

.selectByMsLevel=function(db, level) {

    if (.self$.checkFields('ms.level', fail=FALSE))
        db <- db[db[[.self$.fields[['ms.level']]]] == level, , drop=FALSE]
    else
        db <- db[integer(), , drop=FALSE]

    return(db)
},

.doSelect=function(db, mode=NULL, compound.ids=NULL, mz.min=NULL,
                   mz.max=NULL, min.rel.int=NA_real_, precursor=FALSE, level=0)
{

    # Filtering
    if ( ! is.null(mode) && ! is.na(mode))
        db <- .self$.selectByMode(db, mode)
    if ( ! is.null(compound.ids))
        db <- .self$.selectByCompoundIds(db, compound.ids)
    if ( ! is.null(mz.min) || ! is.null(mz.max))
        db <- .self$.selectByMzValues(db, mz.min, mz.max)
    if ( ! is.na(min.rel.int))
        db <- .self$.selectByRelInt(db, min.rel.int)
    if (precursor)
        db <- .self$.selectByPrecursors(db)
    if (level > 0)
        db <- .self$.selectByMsLevel(db, level)

    return(db)
},

.doSearchMzRange=function(mz.min, mz.max, min.rel.int, ms.mode, max.results,
                          precursor, ms.level) {
    # Overrides super class' method.
    return(.self$.select(mz.min=mz.min, mz.max=mz.max, min.rel.int=min.rel.int,
                         mode=ms.mode, max.rows=max.results, cols='accession',
                         drop=TRUE, uniq=TRUE, sort=TRUE, precursor=precursor,
                         level=ms.level))
},

.doGetMzValues=function(ms.mode, max.results, precursor, ms.level) {
    # Overrides super class' method.

    # Get mz values
    mzcol <- .self$getMatchingMzField()
    mz <- .self$.select(cols=mzcol, mode=ms.mode, drop=TRUE, uniq=TRUE,
                        sort=TRUE, max.rows=max.results, precursor=precursor,
                        level=ms.level)

    return(mz)
}
))
