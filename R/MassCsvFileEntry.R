#' Mass CSV File entry class.
#'
#' This is the entry class for Mass CSV file databases.
#'
#' @seealso Super class \code{\link{BiodbCsvEntry}}.
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
#' @include BiodbCsvEntry.R
#' @export MassCsvFileEntry
#' @exportClass MassCsvFileEntry
MassCsvFileEntry <- methods::setRefClass("MassCsvFileEntry",
    contains='BiodbCsvEntry',

methods=list(

initialize=function(...) {

    callSuper(sep="\t", ...)
},

.parseChromatoCols=function() {

    if (.self$hasField('chrom.col.name') && ! .self$hasField('chrom.col.id'))
        .self$setFieldValue('chrom.col.id',
            .self$getFieldValue('chrom.col.name'))

    if ( ! .self$hasField('chrom.col.name') && .self$hasField('chrom.col.id'))
        .self$setFieldValue('chrom.col.name',
            .self$getFieldValue('chrom.col.id'))
},

.parsePrecursor=function() {

    if ( ! .self$hasField('msprecmz') && .self$hasField('peak.attr')) {
        pkmz <- .self$getFieldValue('peak.mz')
        pkattr <- .self$getFieldValue('peak.attr')
        precursors.attr <- .self$getParent()$getPrecursorFormulae()
        precursors <- pkattr %in% precursors.attr
        # Select the unique precursor found
        if (sum(precursors) == 1)
            .self$setFieldValue('msprecmz', pkmz[precursors])

        # Select peak with highest intensity for precursor
        else if (sum(precursors) > 1) {
            warn0("Found more than one precursor inside entry ",
                .self$getFieldValue('accession', compute=FALSE),
                ': ', paste(pkattr[precursors], collapse=", "),
                ". Trying to take the one with highest intensity.")
            strongest.precursor.mz <- NULL
            for (int.col in c('peak.intensity', 'peak.relative.intensity'))
                if (.self$hasField(int.col)) {
                    int <- .self$getFieldValue(int.col)
                    s <- which(order(int[precursors], decreasing=TRUE) == 1)
                    strongest.precursor.mz <- pkmz[precursors][[s]]
                }
            if (is.null(strongest.precursor.mz))
                warn0('No intensity information found for choosing',
                    ' the strongest precursor.')
            else {
                logInfo('Found strongest precursor: %g.',
                    strongest.precursor.mz)
                .self$setFieldValue('msprecmz', strongest.precursor.mz)
            }
        }
    }
},

.parseFieldsStep2=function(parsed.content) {

    # Check peaks table
    mz <- .self$getFieldValue('peak.mz')
    peaks <- .self$getFieldValue('peaks')
    if ( ! is.null(mz)) {
        if (is.null(peaks))
            error0('No peaks table while a peak.mz field exists for entry ',
                .self$getFieldValue('accession'), ' .')
        if (nrow(peaks) != length(mz))
            error0('Peaks table (nrow=', nrow(peaks), ') does not have ',
                'the same number of elements as peak.mz field (',
                length(mz), ') for entry ', .self$getFieldValue('accession'),
                '.')
    }

    # Chromatographic column id and name
    .self$.parseChromatoCols()

    # Set precursor
    .self$.parsePrecursor()
}

))
