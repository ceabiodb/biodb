# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbCompounddbConn {{{1
################################################################################

#' An interface for all compound databases.
#'
#' This interface must be inherited by all compound databases. It declares
#' method headers specific to compound databases.
#'
#' @seealso \code{\link{BiodbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get the connector of a compound database
#' conn <- mybiodb$getFactory()$createConn('chebi')
#'
#' # Search for compounds
#' conn$searchCompound(name='prion protein', max.results=10)
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbConn.R
#' @export BiodbCompounddbConn
#' @exportClass BiodbCompounddbConn
BiodbCompounddbConn <- methods::setRefClass("BiodbCompounddbConn",
    contains="BiodbConn",

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbCompounddbConn')
},

# Search compound {{{3
################################################################################

searchCompound=function(name=NULL, mass=NULL, mass.field=NULL,
                        mass.tol=0.01, mass.tol.unit='plain',
                        max.results=NA_integer_) {
    "Search for compounds by name and/or by mass. At least one of name or mass
    must be set.
    \nname: The name of a compound to search for.
    \nmass: The searched mass.
    \nmass.field: For searching by mass, you must indicate a mass field to use
    ('monoisotopic.mass', 'molecular.mass', 'average.mass' or 'nominal.mass').
    \nmass.tol: The tolerance value on the molecular mass.
    \nmass.tol.unit: The type of mass tolerance. Either 'plain' or 'ppm'.
    \nmax.results: The maximum number of matches to return.
    \nReturned value: A character vector of entry IDs."

    .self$.abstractMethod()
},

# Annotate M/Z values {{{3
################################################################################

annotateMzValues=function(x, mz.tol, ms.mode, mz.tol.unit=c('plain', 'ppm'),
                          mass.field='monoisotopic.mass',
                          max.results=NA_integer_, mz.col='mz', fields=NULL) {
    "Annotate 
    \nx: Either a data frame or a numeric vector.
    \nfields: A character vector containing the additional entry fields you
    would like to get for each matched entry. Each field will be output in a
    different column.
    \nmass.field: The mass field to use for matching M/Z values. One of:
    'monoisotopic.mass', 'molecular.mass', 'average.mass', 'nominal.mass'.
    \nmax.results: If set, it is used to limit the number of matches found for
    each M/Z value.
    \nms.mode: The MS mode. Set it to either 'neg' or 'pos'.
    \nmz.col: The name of the column where to find M/Z values in case x is a
    data frame.
    \nmz.tol.unit: The type of the M/Z tolerance. Set it to either to 'ppm' or
    'plain'.
    \nReturned value: A data frame containing the input values, and annotation
    columns appended at the end. The first annotation column contains the IDs
    of the matched entries. The following columns contain the fields you have
    requested through the `fields` parameter.
    "
    
    ret <- NULL
 
    # Convert x to data frame
    if ( ! is.data.frame(x))
        x <- data.frame(mz = x)
    
    # Check that we find the M/Z column
    if ( ! mz.col %in% names(x))
        .self$error('No column named "', mz.col,
                    '" was found inside data frame.')
    
    # Check mass field
    mass.fields <- .self$getBiodb()$getEntryFields()$getFieldNames('mass')
    .self$.assertIn(mass.field, mass.fields)
    
    # Get proton mass
    pm <- .self$getBiodb()$getConfig()$get('proton.mass')
        
    # Compute masses
    mass <- x[[mz.col]] + pm * (if (ms.mode == 'neg') +1.0 else -1.0)
    
    # Loop on all masses
    
        # Search for compounds matching this mass
    
        # Build local data frame
    
        # Additional fields required?
    
            # Get entries
    
            # Get values of fields
    
            # Add values to local data frame
    
        # Append local data frame to main data frame
    
    return(ret)
}
                          
                          
# Private methods {{{2
################################################################################

# Check mass field {{{3
################################################################################

.checkMassField=function(mass, mass.field) {

    if ( ! is.null(mass)) {
        .self$.assertIs(mass, c('integer', 'numeric'))
        .self$.assertNotNull(mass.field)
        .self$.assertIs(mass.field, 'character')
        ef <- .self$getBiodb()$getEntryFields()
        mass.fields <- ef$getFieldNames(type='mass')
        .self$.assertIn(mass.field, mass.fields)
    }
}

))
