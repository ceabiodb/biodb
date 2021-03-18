#' {{dbTitle}} entry class.
#'
#' This is the entry class for {{dbTitle}}. 
#'
#' @seealso
#' $$$ CASE MOTHER_CLASS PLAIN $$$
#' \code{\link{BiodbEntry}}.
#' $$$ CASE MOTHER_CLASS CSV $$$
#' \code{\link{BiodbCsvEntry}}.
#' $$$ CASE MOTHER_CLASS HTML $$$
#' \code{\link{BiodbHtmlEntry}}.
#' $$$ CASE MOTHER_CLASS JSON $$$
#' \code{\link{BiodbJsonEntry}}.
#' $$$ CASE MOTHER_CLASS LIST $$$
#' \code{\link{BiodbListEntry}}.
#' $$$ CASE MOTHER_CLASS SDF $$$
#' \code{\link{BiodbSdfEntry}}.
#' $$$ CASE MOTHER_CLASS TXT $$$
#' \code{\link{BiodbTxtEntry}}.
#' $$$ CASE MOTHER_CLASS XML $$$
#' \code{\link{BiodbXmlEntry}}.
#' $$$ END_CASE MOTHER_CLASS $$$
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
#' @export {{entryClass}}
#' @exportClass {{entryClass}}
{{entryClass}} <- methods::setRefClass("{{entryClass}}",
    contains=c(
# $$$ CASE MOTHER_CLASS PLAIN $$$
        'BiodbEntry'
# $$$ CASE MOTHER_CLASS CSV $$$
        'BiodbCsvEntry'
# $$$ CASE MOTHER_CLASS HTML $$$
        'BiodbHtmlEntry'
# $$$ CASE MOTHER_CLASS JSON $$$
        'BiodbJsonEntry'
# $$$ CASE MOTHER_CLASS LIST $$$
        'BiodbListEntry'
# $$$ CASE MOTHER_CLASS SDF $$$
        'BiodbSdfEntry'
# $$$ CASE MOTHER_CLASS TXT $$$
        'BiodbTxtEntry'
# $$$ CASE MOTHER_CLASS XML $$$
        'BiodbXmlEntry'
# $$$ END_CASE MOTHER_CLASS $$$
    ),

methods=list(

initialize=function(...) {
# $$$ CASE MOTHER_CLASS CSV $$$
    callSuper(sep="\t", ...)
# $$$ CASE MOTHER_CLASS DEFAULT $$$
    callSuper(...)
# $$$ END_CASE MOTHER_CLASS $$$
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
