# vi: fdm=marker ts=4 et cc=80 

# KeggOrthologyEntry {{{1
################################################################################

#' @include KeggEntry.R
KeggOrthologyEntry <- methods::setRefClass("KeggOrthologyEntry",
    contains='KeggEntry',

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {
    callSuper(...)
},

# Parse fields step 2 {{{3
################################################################################

.parseFieldsStep2=function(parsed.content) {
}

))
