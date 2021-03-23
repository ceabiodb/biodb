#' {{dbTitle}} entry class.
#'
#' This is the entry class for {{dbTitle}}. 
#'
#' @seealso
#' $$$ CASE ENTRY_TYPE PLAIN $$$
#' \code{\link{BiodbEntry}}.
#' $$$ CASE ENTRY_TYPE CSV $$$
#' \code{\link{BiodbCsvEntry}}.
#' $$$ CASE ENTRY_TYPE HTML $$$
#' \code{\link{BiodbHtmlEntry}}.
#' $$$ CASE ENTRY_TYPE JSON $$$
#' \code{\link{BiodbJsonEntry}}.
#' $$$ CASE ENTRY_TYPE LIST $$$
#' \code{\link{BiodbListEntry}}.
#' $$$ CASE ENTRY_TYPE SDF $$$
#' \code{\link{BiodbSdfEntry}}.
#' $$$ CASE ENTRY_TYPE TXT $$$
#' \code{\link{BiodbTxtEntry}}.
#' $$$ CASE ENTRY_TYPE XML $$$
#' \code{\link{BiodbXmlEntry}}.
#' $$$ END_CASE ENTRY_TYPE $$$
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get a connector that inherits from {{connClass}}:
#' conn <- mybiodb$getFactory()$createConn('{{dbName}}')
#'
#' # Get the first entry
#' e <- conn$getEntry(conn$getEntryIds(1L))
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import biodb
#' @export {{entryClass}}
#' @exportClass {{entryClass}}
{{entryClass}} <- methods::setRefClass("{{entryClass}}",
    contains=c(
# $$$ CASE ENTRY_TYPE PLAIN $$$
        'BiodbEntry'
# $$$ CASE ENTRY_TYPE CSV $$$
        'BiodbCsvEntry'
# $$$ CASE ENTRY_TYPE HTML $$$
        'BiodbHtmlEntry'
# $$$ CASE ENTRY_TYPE JSON $$$
        'BiodbJsonEntry'
# $$$ CASE ENTRY_TYPE LIST $$$
        'BiodbListEntry'
# $$$ CASE ENTRY_TYPE SDF $$$
        'BiodbSdfEntry'
# $$$ CASE ENTRY_TYPE TXT $$$
        'BiodbTxtEntry'
# $$$ CASE ENTRY_TYPE XML $$$
        'BiodbXmlEntry'
# $$$ END_CASE ENTRY_TYPE $$$
    ),

methods=list(

initialize=function(...) {
# $$$ CASE ENTRY_TYPE CSV $$$
    callSuper(sep="\t", ...)
# $$$ CASE ENTRY_TYPE DEFAULT $$$
    callSuper(...)
# $$$ END_CASE ENTRY_TYPE $$$
}

,.isContentCorrect=function(content) {
 
    correct <- callSuper(content)
    
    # You can do some more checks of the content here.
    
    return(correct)
}

,.parseFieldsStep2=function(parsed.content) {
    
    # TODO Implement your custom parsing processing here.
}
))
