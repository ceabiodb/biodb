# vi: fdm=marker ts=4 et cc=80 tw=80

# MassSqliteEntry {{{1
################################################################################

#' Mass SQLite entry class.
#'
#' @include BiodbListEntry.R
#' @export MassSqliteEntry
#' @exportClass MassSqliteEntry
MassSqliteEntry <- methods::setRefClass("MassSqliteEntry",
    contains="BiodbListEntry")
