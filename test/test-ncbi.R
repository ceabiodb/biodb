#!/usr/bin/env R --slave -f
library(RUnit)
source('../NcbiConn.R', chdir = TRUE)

entries <- list('9606' = list(),
                '2139485387547754' = list(false = TRUE),
                '7273' = list(big = TRUE),
                '3627' = list(symbol = 'CXCL10'))

# Open connexion
conn <- NcbiConn$new(useragent = "fr.cea.test-ncbi ; pierrick.rogermele@cea.fr")

# Loop on all entries
for (id in names(entries)) {
	print(id)
	entry <- conn$getGeneEntry(id)
	print(entries[[id]])
	print(names(entries[[id]]))
	print(which(names(entries[[id]]) == 'false'))
	if (length(which(names(entries[[id]]) == 'false')) > 0)
		print(entries[[id]][['false']])
}

# Get a real entry
id <- 9606
entry <- conn$getGeneEntry(id)
checkTrue( ! is.null(entry))
checkEquals(entry$getId(), id)

# Try to get an non-existing entry
entry <- conn$getGeneEntry(2139485387547754)
checkTrue(is.null(entry))

# Get another entry
#id <- 7273
#entry <- conn$getGeneEntry(id)
#checkTrue( ! is.null(entry))
#checkEquals(entry$getId(), id)

# Get another entry
id <- 3627
entry <- conn$getGeneEntry(id)
checkTrue( ! is.null(entry))
checkEquals(entry$getId(), id)
checkEquals(entry$getSymbol(), 'CXCL10')
entry$save('3627.xml')
