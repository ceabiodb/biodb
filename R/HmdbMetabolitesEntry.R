# vi: fdm=marker ts=4 et cc=80 tw=80

# HmdbMetabolitesEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' HMDB Metabolites entry class.
#'
#' This is the entry class for the HMDB Metabolites database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('hmdb.metabolites')
#'
#' # Get an entry
#' \dontrun{
#' e <- conn$getEntry('HMDB0000001')
#' }
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbXmlEntry.R
#' @export HmdbMetabolitesEntry
#' @exportClass HmdbMetabolitesEntry
HmdbMetabolitesEntry <- methods::setRefClass("HmdbMetabolitesEntry",
    contains="BiodbXmlEntry",

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
},

# Private methods {{{2
################################################################################

# Is parsed content correct {{{3
################################################################################

.isParsedContentCorrect=function(parsed.content) {
    return(length(XML::getNodeSet(parsed.content, "//error")) == 0)
},

# Parse fields step 2 {{{3
################################################################################

.parseFieldsStep2=function(parsed.content) {

    # Remove fields with empty string
    for (f in .self$getFieldNames()) {
        v <- .self$getFieldValue(f)
        if (is.character(v) && ! is.na(v) && v == '')
            .self$removeField(f)
    }

    # Correct InChIKey
    if (.self$hasField('INCHIKEY')) {
        v <- sub('^InChIKey=', '', .self$getFieldValue('INCHIKEY'), perl=TRUE)
        .self$setFieldValue('INCHIKEY', v)
    }

    # Synonyms
    synonyms <- XML::xpathSApply(parsed.content, "//synonym", XML::xmlValue)
    if (length(synonyms) > 0)
        .self$appendFieldValue('name', synonyms)
}

))
