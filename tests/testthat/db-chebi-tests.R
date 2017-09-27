# vi: fdm=marker

# Test ChEBI encoding issue in XML {{{1
################################################################

test.chebi.encoding.issue.in.xml <- function(db) {

	entry <- db$getBiodb()$getFactory()$getEntry(db$getId(), '2571') # +- sign (U+00b1) appears as <c2><b1>: "<chebiName>(<c2><b1>)-2-Heptanol</chebiName>" instead of "<chebiName>(±)-2-Heptanol</chebiName>"
}
