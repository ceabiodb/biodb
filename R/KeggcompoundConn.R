# vi: fdm=marker

# Class declaration {{{1
################################################################

#'KEGG Compound connection class.
#'@export
KeggcompoundConn <- methods::setRefClass("KeggcompoundConn", contains = "KeggConn")

# Constructor {{{1
################################################################

KeggcompoundConn$methods( initialize = function(...) {
	callSuper(db.name = 'compound', db.abbrev = 'cpd', ...)
})

# Create entry {{{1
################################################################

KeggcompoundConn$methods( createEntry = function(content, drop = TRUE) {

	entries <- list()

	# Define fields regex
	regex <- character()
	regex[[BIODB.NAME]] <- "^NAME\\s+([^,;]+)"
	regex[[BIODB.CHEBI.ID]] <- "^\\s+ChEBI:\\s+(\\S+)"
	regex[[BIODB.LIPIDMAPSSTRUCTURE.ID]] <- "^\\s+LIPIDMAPS:\\s+(\\S+)"

	for (text in content) {

		# Create instance
		entry <- BiodbEntry$new(biodb = .self$getBiodb())

		lines <- strsplit(text, "\n")
		for (s in lines[[1]]) {

			# Test generic regex
			parsed <- FALSE
			for (field in names(regex)) {
				g <- stringr::str_match(s, regex[[field]])
				if ( ! is.na(g[1,1])) {
					entry$setField(field, g[1,2])
					parsed <- TRUE
					break
				}
			}
			if (parsed)
				next

			# ACCESSION
			{
#				# ENZYME ID
#				g <- stringr::str_match(s, "^ENTRY\\s+EC\\s+(\\S+)")
#				if ( ! is.na(g[1,1])){
#					entry$setField(BIODB.ACCESSION, paste('ec', g[1,2], sep = ':'))
#
#				# ENTRY ID
#				}else {
					g <- stringr::str_match(s, "^ENTRY\\s+(\\S+)\\s+Compound")
					if ( ! is.na(g[1,1])){
						entry$setField(BIODB.ACCESSION, g[1,2])

					# OTHER ID
#					}else {
#						g <- stringr::str_match(s, "^ENTRY\\s+(\\S+)")
#						if ( ! is.na(g[1,1]))
#							entry$setField(BIODB.ACCESSION, g[1,2])
#					}
				}

				# ORGANISM
				g <- stringr::str_match(s, "^ORGANISM\\s+(\\S+)")
				if ( ! is.na(g[1,1]))
					entry$setField(BIODB.ACCESSION, paste(g[1,2], entry$getField(BIODB.ACCESSION), sep = ':'))
			}
		}

		entries <- c(entries, entry)
	}

	# Replace elements with no accession id by NULL
	entries <- lapply(entries, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

	# If the input was a single element, then output a single object
	if (drop && length(content) == 1)
		entries <- entries[[1]]

	return(entries)
})
