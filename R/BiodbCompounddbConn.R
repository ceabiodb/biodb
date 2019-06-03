# vi: fdm=marker ts=4 et cc=80

# BiodbCompounddbConn {{{1
################################################################################

#' An interface for all compound databases.
#'
#' This interface must be inherited by all compound databases. It declares
#' method headers specific to compound databases.
#'
#' @param name          The name of a compound.
#' @param mass          The searched mass.
#' @param mass.field    The mass field to use for the search. For instance:
#'                      'molecular.mass', 'monoisotopic.mass', 'average.mass'.
#' @param mass.tol      The tolerance on the molecular mass.
#' @param mass.tol.unit The unit used for molecular mass tolerance. Either
#'                      'plain' or 'ppm'.
#' @param max.results   The maximum number of matches wanted.
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
    .self$.abstract.class('BiodbCompounddbConn')
},

# Search compound {{{3
################################################################################

searchCompound=function(name=NULL, mass=NULL, mass.field=NULL,
                          mass.tol=0.01, mass.tol.unit='plain',
                          max.results=NA_integer_) {
    "Search for compounds by name and/or by mass. For searching by mass, you
    must indicate a mass field to use ('monoisotopic.mass', 'molecular.mass',
                                       ...)"

    .self$.abstract.method()
},

# Private methods {{{2
################################################################################

# Check mass field {{{3
################################################################################

.checkMassField=function(mass, mass.field) {

    if ( ! is.null(mass)) {
        .self$.assert.is(mass, c('integer', 'numeric'))
        .self$.assert.not.null(mass.field)
        .self$.assert.is(mass.field, 'character')
        mass.fields <- .self$getBiodb()$getEntryFields()$getFieldNames(type='mass')
        .self$.assert.in(mass.field, mass.fields)
    }
}

))
