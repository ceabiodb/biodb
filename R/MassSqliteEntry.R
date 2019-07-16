# vi: fdm=marker ts=4 et cc=80 tw=80

# MassSqliteEntry {{{1
################################################################################

#' @include BiodbEntryList.R
MassSqliteEntry <- methods::setRefClass("MassSqliteEntry",
    contains="BiodbEntryList")
