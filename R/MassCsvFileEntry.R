#' Mass CSV File entry class.
#'
#' This is the entry class for MASS CSV file databases.
#'
#' @seealso Super class \code{\link{BiodbCsvEntry}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get path to LCMS database example file
#' lcmsdb <- system.file("extdata", "lcmsdb.tsv", package="biodb")
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

    peaks <- .self$getFieldValue('peaks')

    if ( ! is.null(peaks) && 'peak.attr' %in% colnames(peaks)) {
        precursors.attr <- .self$getParent()$getPrecursorFormulae()
        precursors <- peaks[['peak.attr']] %in% precursors.attr
        if (sum(precursors) == 1)
            .self$setFieldValue('msprecmz', peaks[precursors, 'peak.mz'])
        else if (sum(precursors) > 1) {
            .self$caution("Found more than one precursor inside entry ",
                          .self$getFieldValue('accession', compute=FALSE),
                          ': ', paste(peaks[precursors, 'peak.attr'],
                                      collapse=", "),
                          ". Trying to take the one with highest intensity.")
            strongest.precursor.mz <- NULL
            for (int.col in c('peak.intensity', 'peak.relative.intensity'))
                if (int.col %in% colnames(peaks)) {
                    s <- which(order(peaks[precursors, int.col],
                                     decreasing=TRUE) == 1)
                    strongest.precursor.mz <- peaks[precursors, 'peak.mz'][[s]]
                }
            if (is.null(strongest.precursor.mz))
                .self$caution('No intensity information found for choosing',
                              ' the strongest precursor.')
            else {
                .self$info('Found strongest precursor:',
                           strongest.precursor.mz, '.')
                .self$setFieldValue('msprecmz', strongest.precursor.mz)
            }
        }
    }
},

#.parsePeakTable=function(parsed.content) {
#
#    entry.fields <- .self$getBiodb()$getEntryFields()

    # Make peak table
#    peaks <- NULL
#    for (field in entry.fields$getFieldNames()) {
#
#        # Process only peak fields
#        f <- entry.fields$get(field)
#        if ( ! is.na(f$getGroup()) && f$getGroup() == 'peak') {
#
#            # Is the field is present in the parsed content data frame
#            if (field %in% names(.self$getParent()$.fields))
#                col.name <- .self$getParent()$.fields[[field]]
#            else
#                col.name <- field
#            if (col.name %in% colnames(parsed.content)) {
#
#                # Get vector of values
#                values <- parsed.content[[col.name]]
#
#                # Correct values
#                values <- f$correctValue(values)
#
#                # Add values to peak data frame
#                if (is.null(peaks)) {
#                    peaks <- data.frame(x=values, stringsAsFactors=FALSE)
#                    colnames(peaks) <- field
#                }
#                else
#                    peaks[[field]] <- values
#            }
#        }
#    }

#    # Add MZ column if missing
#    if ( ! is.null(peaks) && ! 'peak.mz' %in% colnames(peaks))
#        for (mz.col in c('peak.mztheo', 'peak.mzexp'))
#            if (mz.col %in% colnames(peaks))
#                peaks[['peak.mz']] <- peaks[[mz.col]]

    # Set peaks table in field
#    if ( ! is.null(peaks))
#        .self$setFieldValue('peaks', peaks)
#},

.parseFieldsStep2=function(parsed.content) {

    # Peak table
#    .self$.parsePeakTable(parsed.content)

    # Chromatographic column id and name
    .self$.parseChromatoCols()

    # Set precursor
    .self$.parsePrecursor()
}

))
