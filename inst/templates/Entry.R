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
#' @import R6
#' @export
{{entryClass}} <- R6::R6Class("{{entryClass}}",
    inherit=
# $$$ CASE ENTRYTYPE PLAIN $$$
        biodb::BiodbEntry
# $$$ CASE ENTRYTYPE CSV $$$
        biodb::BiodbCsvEntry
# $$$ CASE ENTRYTYPE HTML $$$
        biodb::BiodbHtmlEntry
# $$$ CASE ENTRYTYPE JSON $$$
        biodb::BiodbJsonEntry
# $$$ CASE ENTRYTYPE LIST $$$
        biodb::BiodbListEntry
# $$$ CASE ENTRYTYPE SDF $$$
        biodb::BiodbSdfEntry
# $$$ CASE ENTRYTYPE TXT $$$
        biodb::BiodbTxtEntry
# $$$ CASE ENTRYTYPE XML $$$
        biodb::BiodbXmlEntry
# $$$ END_CASE ENTRYTYPE $$$
    ,

public=list(

initialize=function(...) {
# $$$ CASE ENTRYTYPE CSV $$$
    super$initialize(sep="\t", ...)
# $$$ CASE ENTRYTYPE DEFAULT $$$
    super$initialize(...)
# $$$ END_CASE ENTRYTYPE $$$
}

,isContentCorrect=function(content) {
 
    correct <- callSuper(content)
    
    # You can do some more checks of the content here.
    
    return(correct)
}

,parseFieldsStep2=function(parsed.content) {
    
    # TODO Implement your custom parsing processing here.
}
))
