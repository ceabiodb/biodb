#!/usr/bin/env R --slave -f
library(RUnit)
source('../NcbiConn.R', chdir = TRUE)

# Open connexion
conn <- NcbiConn$new(useragent = "fr.cea.test-ncbi ; pierrick.rogermele@cea.fr")

# Get real entry
entry <- conn$getGeneEntry(9606)
checkEquals(entry$getId(), 9606)

# Try to get an non-existing entry
entry <- conn$getGeneEntry(2139485387547754)
checkTrue(is.null(entry))
