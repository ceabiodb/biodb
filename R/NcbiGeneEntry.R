# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiGeneEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' NCBI Gene entry class.
#'
#' This is the entry class for a NCBI Gene database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('ncbi.gene')
#'
#' # Get an entry
#' e <- conn$getEntry('2833')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbXmlEntry.R
#' @export NcbiGeneEntry
#' @exportClass NcbiGeneEntry
NcbiGeneEntry <- methods::setRefClass("NcbiGeneEntry",
    contains="BiodbXmlEntry",

# Private methods {{{2
################################################################################

methods=list(

# Is parsed content correct {{{3
################################################################################

.isParsedContentCorrect=function(parsed.content) {

    n <- XML::getNodeSet(parsed.content, "//Error")
    if (length(n) == 0)
        n <- XML::getNodeSet(parsed.content, "//ERROR")

    return(length(n) == 0)
},

# Parse fields step 2 {{{3
################################################################################

.parseFieldsStep2=function(parsed.content) {

    # CCDS ID
    ccdsid <- .self$.findCcdsId(parsed.content)
    if ( ! is.na(ccdsid))
        .self$setFieldValue('ncbi.ccds.id', ccdsid)
},

# Find ccds id {{{3
################################################################################

.findCcdsId=function(parsed.content) {

    # 1) Get all CCDS tags.
    xpath <- "//Dbtag_db[text()='CCDS']/..//Object-id_str"
    ccds_elements <- XML::getNodeSet(parsed.content, xpath)

    # 2) If all CCDS are the same, go to point 3.
    ccds <- NA_character_
    for (e in ccds_elements) {
        current_ccds <- XML::xmlValue(e)
        if (is.na(ccds))
            ccds <- current_ccds
        else {
            if (current_ccds != ccds) {
                ccds <- NA_character_
                break
            }
        }
    }

    # 3) There are several CCDS values, we need to find the best one (i.e.: the
    # most current one).
    if (is.na(ccds)) {
        # For each CCDS, look for the parent Gene-commentary tag. Then look for
        # the text content of the Gene-commentary_label which is situed under.
        # Ignore CCDS that have no Gene-commentary_label associated. Choose the
        # CCDS that has the smallest Gene-commentary_label in alphabetical
        # order.
        version <- NA_character_
        for (e in ccds_elements) {
            xpath <- "ancestor::Gene-commentary/Gene-commentary_label"
            versions <- XML::xpathSApply(e, xpath, XML::xmlValue)
            if (length(versions) < 1) next
            current_version <- versions[[length(versions)]]
            if (is.na(version) || current_version < version) {
                version <- current_version
                ccds <- XML::xmlValue(e)
            }
        }
    }

    return(ccds)
}

))
