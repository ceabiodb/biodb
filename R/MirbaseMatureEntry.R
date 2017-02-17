#####################
# CLASS DECLARATION #
#####################

MirbaseMatureEntry <- methods::setRefClass("MirbaseMatureEntry", contains = "BiodbEntry")

###########
# FACTORY #
###########

createMirbaseMatureEntryFromTxt <- function(biodb, contents, drop = TRUE) {

	entries <- list()

	# Define fields regex
	regex <- character()
	regex[[BIODB.ACCESSION]] <- "^>[^ ]+ *(MIMAT[0-9]+) .*$"
	regex[[BIODB.NAME]] <- "^>([^ ]+) *MIMAT[0-9]+ .*$"
	regex[[BIODB.SEQUENCE]] <- "^([ACGU]+)$"

	for (text in contents) {

		# Create instance
		entry <- MirbaseMatureEntry$new(biodb)

		lines <- strsplit(text, "\n")
		for (s in lines[[1]]) {

			# Test generic regex
			for (field in names(regex)) {
				g <- stringr::str_match(s, regex[[field]])
				if ( ! is.na(g[1,1]))
					entry$setField(field, g[1,2])
			}
		}

		entries <- c(entries, entry)
	}

	# Replace elements with no accession id by NULL
	entries <- lapply(entries, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

	# If the input was a single element, then output a single object
	if (drop && length(contents) == 1)
		entries <- entries[[1]]

	return(entries)
}
