#!/usr/bin/env R --slave -f
library(RUnit)
source('../NcbiConn.R', chdir = TRUE)

full_test <- FALSE
args <- commandArgs(trailingOnly = TRUE)
full_test = args[1]

entries <- list('9606' = list(),
                '2139485387547754' = list(false = TRUE),
                '7273' = list(big = TRUE),
                '3627' = list(symbol = 'CXCL10'))

# Function for testing if a key exists inside a list/hashmap
hHasKey <- function(h, k) {
	return(length(which(names(h) == k)) > 0)
}

# Function for getting a boolean value from a list/hashmap
hGetBool <- function(h, k) {
	if (hHasKey(h, k)) return(h[[k]]) else return(FALSE)
}

# Open connexion
conn <- NcbiConn$new(useragent = "fr.cea.test-ncbi ; pierrick.rogermele@cea.fr")

# Loop on all entries
for (id in names(entries)) {

	# Skip big entry (take too much time)
	if (hGetBool(entries[[id]], 'big') && ! full_test)
		next

	# Get Entry from database
	entry <- conn$getGeneEntry(id)

	# This is a false entry => test that it's null
	if (hGetBool(entries[[id]], 'false'))
		checkTrue(is.null(entry))

	# This is a real entry => test that it isn't null
	else {
		checkTrue( ! is.null(entry))

		# Check that returned id is the same
		checkEquals(entry$getId(), id)
		
		# Check symbol
		if (hHasKey(entries[[id]], 'symbol'))
			checkEquals(entry$getSymbol(), entries[[id]][['symbol']])

		# save
		entry$save(id.'.xml')
	}
}

# Get a real entry
# id <- 9606
# entry <- conn$getGeneEntry(id)
# checkTrue( ! is.null(entry))
# checkEquals(entry$getId(), id)

# Try to get an non-existing entry
# entry <- conn$getGeneEntry(2139485387547754)
# checkTrue(is.null(entry))

# Get another entry
#id <- 7273
#entry <- conn$getGeneEntry(id)
#checkTrue( ! is.null(entry))
#checkEquals(entry$getId(), id)

# Get another entry
# id <- 3627
# entry <- conn$getGeneEntry(id)
# checkTrue( ! is.null(entry))
# checkEquals(entry$getId(), id)
# checkEquals(entry$getSymbol(), 'CXCL10')
# entry$save('3627.xml')
