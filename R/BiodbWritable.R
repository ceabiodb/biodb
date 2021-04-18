#' An abstract class (more like an interface) to model a writable database.
#'
#' A database class that implements this interface must allow the addition of
#' new entries.
#'
#' @seealso \code{\link{BiodbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create an empty MASS SQLite database
#' mydb <- mybiodb$getFactory()$createConn('mass.sqlite')
#'
#' # Create new entry object
#' entry <- mybiodb$getFactory()$createNewEntry('mass.sqlite')
#' entry$setFieldValue('accession', '0')
#' entry$setFieldValue('name', 'Some Entry')
#'
#' # Add the new entry
#' mydb$allowEditing()
#' mydb$addNewEntry(entry)
#'
#' # Write the database
#' mydb$allowWriting()
#' mydb$setUrl('base.url', 'mydatabase.sqlite')
#' mydb$write()
#'
#' # Terminate instance.
#' mybiodb$terminate()
#' mybiodb <- NULL
#'
#' @import methods
#' @include BiodbObject.R
#' @export BiodbWritable
#' @exportClass BiodbWritable
BiodbWritable <- methods::setRefClass("BiodbWritable",
    contains='BiodbObject',
    fields=list(
        .writing.allowed='logical'
        ),

methods=list(

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbWritable')

    # This constructor is never called, because this class is used as an
    # interface (i.e.: it is declared in the "contains" field of another class,
    # in second position or greater. Only the constructor of the first declared
    # "contained" class is called.).
},

writingIsAllowed=function() {
    ":\n\nTests if the connector has access right to the database.
    \nReturned value: TRUE if writing is allowed for this database, FALSE
    otherwise.
    "
    
    .self$.initWritable()

    return(.self$.writing.allowed)
},

allowWriting=function() {
    ":\n\nAllows the connector to write into this database.
    \nReturned value: None.
    "

    .self$setWritingAllowed(TRUE)
},

disallowWriting=function() {
    ":\n\nDisallows the connector to write into this database.
    \nReturned value: None.
    "
    
    .self$setWritingAllowed(FALSE)
},

setWritingAllowed=function(allow) {
    ":\n\nAllows or disallows writing for this database.
    \nallow: If set to TRUE, allows writing.
    \nReturned value: None.
    "
    
    chk::chk_logical(allow)
    .self$.writing.allowed <- allow
},

write=function() {
    ":\n\nWrites into the database. All modifications made to the database since
    the last time write() was called will be saved.
    \nReturned value: None.
    "

    .self$.checkWritingIsAllowed()
    .self$.doWrite()

    # Unset "new" flag for all entries
    for (e in .self$.entries)
        e$.setAsNew(FALSE)
},

.checkWritingIsAllowed=function() {
    
    .self$.initWritable()
    
    if ( ! .self$.writing.allowed)
        fatal('Writing is not enabled for this database. However this',
              ' database type is writable. Please call allowWriting()',
              ' method to enable writing.', fmt='paste0')
},

.doWrite=function() {
    .self$.abstractMethod()
},

.initWritable=function() {
    if (length(.self$.writing.allowed) == 0)
        .self$setWritingAllowed(FALSE)
}

))
