# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbEditable {{{1
################################################################################

# Declaration {{{2
################################################################################

#' An interface to model an editable database.
#'
#' A database class that implements this interface allows the addition of new
#' entries, the removal of entries and the modification of entries. If you want
#' your modifications to be persistent, the database class must also be writable
#' (see interface \code{BiodbWritable}), you must call the method \code{write}
#' on the connector.
#'
#' @seealso \code{\link{BiodbConn}} and \code{\link{BiodbWritable}}.
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
#' # Terminate instance.
#' mybiodb$terminate()
#' mybiodb <- NULL
#'
#' @import methods
#' @include BiodbObject.R
#' @export BiodbEditable
#' @exportClass BiodbEditable
BiodbEditable <- methods::setRefClass("BiodbEditable",
    contains='BiodbObject',
    fields=list(
        .editing.allowed='logical'
    ),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbEditable')

    # This constructor is never called, because this class is used as an
    # interface (i.e.: it is declared in the "contains" field of another class,
    # in second position or greater. Only the constructor of the first declared
    # "contained" class is called.).
},

# Editing is allowed {{{3
################################################################################

editingIsAllowed=function() {
    ":\n\nTests if editing is allowed.
    \nReturned value: TRUE if editing is allowed for this database, FALSE
    otherwise.
    "
    
    .self$.initEditable()

    return(.self$.editing.allowed)
},

# Allow editing {{{3
################################################################################

allowEditing=function() {
    ":\n\nAllows editing for this database.
    \nReturned value: None.
    "

    .self$setEditingAllowed(TRUE)
},

# Disallow editing {{{3
################################################################################

disallowEditing=function() {
    ":\n\nDisallows editing for this database.
    \nReturned value: None.
    "
    
    .self$setEditingAllowed(FALSE)
},

# Set editing allowed {{{3
################################################################################

setEditingAllowed=function(allow) {
    ":\n\nAllow or disallow editing for this database.
    \nallow: A logical value.
    \nReturned value: None.
    "
    
    .self$.assertIs(allow, 'logical')
    .self$.editing.allowed <- allow
},

# Add new entry {{{3
################################################################################

addNewEntry=function(entry) {
    ":\n\nAdds a new entry to the database. The passed entry must have been
    previously created from scratch using BiodbFactory::createNewEntry() or
    cloned from an existing entry using BiodbEntry::clone().
    \nentry: The new entry to add. It must be a valid BiodbEntry object.
    \nReturned value: None.
    "

    .self$.checkEditingIsAllowed()

    # Is already part of a connector instance?
    if (entry$parentIsAConnector())
        .self$'error'('Impossible to add entry as a new entry. The passed',
                      ' entry is already part of a connector.')

    # No accession number?
    if ( ! entry$hasField('accession'))
        .self$error('Impossible to add entry as a new entry. The passed entry',
                    ' has no accession number.')
    id <- entry$getFieldValue('accession')
    if (is.na(id))
        .self$error('Impossible to add entry as a new entry. The passed',
                    ' entry has an accession number set to NA.')

    # Accession number is already used?
    e <- .self$getEntry(id)
    if ( ! is.null(e))
        .self$error('Impossible to add entry as a new entry. The accession',
                    ' number of the passed entry is already used in the',
                    ' connector.')

    # Make sure ID field is equal to accession
    id.field <- .self$getEntryIdField()
    if ( ! entry$hasField(id.field) || entry$getFieldValue(id.field) != id)
        entry$setFieldValue(id.field, id)

    # Remove entry from non-volatile cache
    cch <- .self$getBiodb()$getPersistentCache()
    if (cch$isWritable())
        cch$deleteFile(.self$getCacheId(), name=id, ext=.self$getEntryFileExt())

    # Flag entry as new
    entry$.setAsNew(TRUE)

    # Set the connector as its parent
    entry$.setParent(.self)

    # Add entry to volatile cache
    .self$.addEntriesToCache(id, list(entry))
},

# Private methods {{{2
################################################################################

# Init parameters {{{3
################################################################################

.initEditable=function() {
    if (length(.self$.editing.allowed) == 0)
        .self$setEditingAllowed(FALSE)
},

# Check that editing is allowed {{{3
################################################################################

.checkEditingIsAllowed=function() {
    
    .self$.initEditable()
    
    if ( ! .self$.editing.allowed)
        .self$error('Editing is not enabled for this database. However this',
                    ' database type is editable. Please call allowEditing()',
                    ' method to enable editing.')
}

))
