# vi: fdm=marker ts=4 et cc=80 tw=80

# LipidmapsStructureEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' Lipidmaps Structure entry class.
#'
#' This is the entry class for Lipidmaps Structure database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('lipidmaps.structure')
#'
#' # Get an entry
#' e <- conn$getEntry('LMFA00000001')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbCsvEntry.R
#' @export LipidmapsStructureEntry
#' @exportClass LipidmapsStructureEntry
LipidmapsStructureEntry <- methods::setRefClass("LipidmapsStructureEntry",
    contains='BiodbCsvEntry',

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(na.strings=c('', '-'), ...)
},

# Private methods {{{2
################################################################################

# Is content correct {{{3
################################################################################

.isContentCorrect=function(content) {
    return( ! grepl("No records? found", content))
},

# Parse fields step 2 {{{3
################################################################################

.parseFieldsStep2=function(parsed.content) {

    # Set synonyms 
    if (.self$hasField('SYNONYMS')) {
        v <- strsplit(.self$getFieldValue('SYNONYMS'), ';')[[1]]
        v <- sub('^ +', '', v, perl=TRUE)
        v <- sub(' +$', '', v, perl=TRUE)
        .self$setFieldValue('SYNONYMS', v)
    }

    # Synonyms
    if ('SYNONYMS' %in% names(parsed.content)) {
        v <- parsed.content[['SYNONYMS']]
        if ( ! is.na(v)) {
            v <- strsplit(v, ' *; *')[[1]]
            .self$appendFieldValue('name', v)
        }
    }
}

))
