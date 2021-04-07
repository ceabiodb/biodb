#' {{dbTitle}} entry class.
#'
#' Entry class for {{dbTitle}}. 
#'
#' @seealso
#' $$$ CASE ENTRYTYPE PLAIN $$$
#' \code{\link{BiodbEntry}}.
#' $$$ CASE ENTRYTYPE CSV $$$
#' \code{\link{BiodbCsvEntry}}.
#' $$$ CASE ENTRYTYPE HTML $$$
#' \code{\link{BiodbHtmlEntry}}.
#' $$$ CASE ENTRYTYPE JSON $$$
#' \code{\link{BiodbJsonEntry}}.
#' $$$ CASE ENTRYTYPE LIST $$$
#' \code{\link{BiodbListEntry}}.
#' $$$ CASE ENTRYTYPE SDF $$$
#' \code{\link{BiodbSdfEntry}}.
#' $$$ CASE ENTRYTYPE TXT $$$
#' \code{\link{BiodbTxtEntry}}.
#' $$$ CASE ENTRYTYPE XML $$$
#' \code{\link{BiodbXmlEntry}}.
#' $$$ END_CASE ENTRYTYPE $$$
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
# $$$ CASE ENTRYTYPE PLAIN $$$
        'BiodbEntry'
# $$$ CASE ENTRYTYPE CSV $$$
        'BiodbCsvEntry'
# $$$ CASE ENTRYTYPE HTML $$$
        'BiodbHtmlEntry'
# $$$ CASE ENTRYTYPE JSON $$$
        'BiodbJsonEntry'
# $$$ CASE ENTRYTYPE LIST $$$
        'BiodbListEntry'
# $$$ CASE ENTRYTYPE SDF $$$
        'BiodbSdfEntry'
# $$$ CASE ENTRYTYPE TXT $$$
        'BiodbTxtEntry'
# $$$ CASE ENTRYTYPE XML $$$
        'BiodbXmlEntry'
# $$$ END_CASE ENTRYTYPE $$$
    ),

methods=list(

initialize=function(...) {
# $$$ CASE ENTRYTYPE CSV $$$
    callSuper(sep="\t", ...)
# $$$ CASE ENTRYTYPE DEFAULT $$$
    callSuper(...)
# $$$ END_CASE ENTRYTYPE $$$
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
