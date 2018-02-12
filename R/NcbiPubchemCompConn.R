# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include NcbiPubchemConn.R
#' @include CompounddbConn.R
NcbiPubchemCompConn <- methods::setRefClass("NcbiPubchemCompConn", contains = c("NcbiPubchemConn", 'CompounddbConn'))

# Constructor {{{1
################################################################

NcbiPubchemCompConn$methods( initialize = function(...) {
	callSuper(db.name = 'compound', id.xmltag = 'PC-CompoundType_id_cid', entry.xmltag = 'PC-Compound', id.urlfield = 'cid', db.entrez.name = 'pccompound', ...)
})

# Search compound {{{1
################################################################

NcbiPubchemCompConn$methods( searchCompound = function(name = NULL, molecular.mass = NULL, monoisotopic.mass = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
	":\n\nSearch for compounds by name and/or by mass."

	term <- character()

	# Search by name
	if ( ! is.null(name))
		term <- paste0('"', name, '"', '[IUPACName]')

	# Search by mass
	if ( ! is.null(monoisotopic.mass) || ! is.null(molecular.mass)) {

		mass <- if (is.null(monoisotopic.mass)) molecular.mass else monoisotopic.mass
		mass.field <- if (is.null(monoisotopic.mass)) 'MolecularWeight' else 'MonoisotopicMass'

		if (mass.tol.unit == 'ppm') {
			mass.min <- mass * (1 - mass.tol * 1e-6)
			mass.max <- mass * (1 + mass.tol * 1e-6)
		} else {
			mass.min <- mass - mass.tol
			mass.max <- mass + mass.tol
		}

		mass.term <- paste0(mass.min, ':', mass.max, '[', mass.field, ']')

		if (length(term) > 0)
			term <- paste(term, 'AND', mass.term)
		else
			term <- mass.term
	}

	# Set retmax
	if (is.na(max.results)) {
		xml <- .self$ws.esearch(term = term, retmax = 0, biodb.parse = TRUE)
		retmax <- as.integer(XML::xpathSApply(xml, "/eSearchResult/Count", XML::xmlValue))
		if (length(retmax) == 0)
			retmax = NA_integer_
	}
	else
		retmax <- max.results

	# Send request
	ids <- .self$ws.esearch(term = term, retmax = retmax, biodb.ids = TRUE)

	return(ids)
})
