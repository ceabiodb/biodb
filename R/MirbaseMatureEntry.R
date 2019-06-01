# vi: fdm=marker ts=4 et cc=80

#' @include BiodbTxtEntry.R

# class declaration {{{1
################################################################################

MirbaseMatureEntry <- methods::setRefClass("MirbaseMatureEntry", contains = "BiodbTxtEntry")

# Initialize {{{1
################################################################################

MirbaseMatureEntry$methods( initialize = function(...) {

    callSuper(...)
})
