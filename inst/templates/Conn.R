#' {{dbTitle}} connector class.
#'
#' This is the connector class for {{dbTitle}}.
#'
#' @seealso 
#' $$$CASE MOTHER_CLASS PLAIN$$$
#' \code{\link{BiodbConn}}
#' $$$CASE MOTHER_CLASS COMPOUND$$$
#' \code{\link{BiodbCompounddbConn}}
#' $$$CASE MOTHER_CLASS MASS$$$
#' \code{\link{BiodbMassdbConn}}
#' $$$END_CASE$$$
#' $$$SECTION WRITABLE$$$
#' ,\code{\link{BiodbWritable}}
#' $$$END_SECTION$$$
#' $$$SECTION EDITABLE$$$
#' ,\code{\link{BiodbEditable}}
#' $$$END_SECTION$$$
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get a connector:
#' conn <- mybiodb$getFactory()$createConn('{{dbName}}')
#'
#' # Get the first entry
#' e <- conn$getEntry(conn$getEntryIds(1L))
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @export {{connClass}}
#' @exportClass {{connClass}}
{{connClass}} <- methods::setRefClass("{{connClass}}",
    contains=c(
# $$$CASE MOTHER_CLASS PLAIN$$$
        "BiodbConn"
# $$$CASE MOTHER_CLASS COMPOUND$$$
        "BiodbCompounddbConn"
# $$$CASE MOTHER_CLASS MASS$$$
        "BiodbMassdbConn"
# $$$END_CASE$$$
# $$$SECTION EDITABLE$$$
        ,'BiodbEditable'
# $$$END_SECTION$$$
# $$$SECTION WRITABLE$$$
        ,'BiodbWritable'
# $$$END_SECTION$$$
    ),
    fields=list(
    ),

methods=list(

initialize=function(...) {
    callSuper(...)
}

,getNbEntries=function(count=FALSE) {
    # Overrides super class' method.

    # Replace the call below if you have a direct way (specific web service for
    # a remote database, provided method or information for a local database)
    # to count entries for your database.
    return(callSuper(count=count))
}

,getEntryContentFromDb=function(entry.id) {
    # Overrides super class' method.

    # Initialize return values
    content <- rep(NA_character_, length(entry.id))

    # TODO Implement retrieval of entry contents.

    # Some debug message
    if (length(content) > 0)
        .self$message('debug', paste("Content of first entry:", content[[1]]))

    return(content)
}

,.doGetEntryIds=function(max.results=NA_integer_) {
    # Overrides super class' method.

    ids <- NA_character_
 
    # TODO Implement retrieval of accession numbers.
    
    return(ids)
}

# $$$SECTION WRITABLE$$$
,.doWrite=function() {
    # Overrides super class' method.

    # TODO Update database on disk by writing new entries into it.
    # You have the choice between writing only the new entries or rewriting all
    # entries, depending on the type of the database (e.g.: CSV file or SQL
    # database for instance)

    # --- FIRST CHOICE --- WRITING DOWN NEW ENTRIES ONLY (SQL database case)
    # If you just need to write new entries, then loop on all entries in memory
    # and use the `isNew()` method on each entry to now its status. 
    newEntries <- Filter(function(e) e$isNew,
                         .self$getAllVolatileCacheEntries())
    # TODO Write the new entry to cache

    # TODO Choose between the scheme above or the scheme below

    # --- SECOND CHOICE --- REWRITING DOWN ALL ENTRIES (CSV file case)
    # Make sure all entries are loaded into cache.
    entry.ids <- .self$getEntryIds()
    entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), entry.ids)

    # Get all entries: the ones loaded from the database file and the ones
    # created in memory (and not saved).
    allEntries <- .self$getAllCacheEntries()

    # Write all entries
    # TODO
}
# $$$END_SECTION$$$
))
