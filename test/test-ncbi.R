#!/usr/bin/env R --slave -f
library(RUnit)
source('../NcbiConn.R', chdir = TRUE)

# Open connexion
conn <- NcbiConn$new(useragent = "fr.cea.test-ncbi ; pierrick.rogermele@cea.fr")

# Get a real entry
id <- 9606
entry <- conn$getGeneEntry(id)
checkTrue( ! is.null(entry))
checkEquals(entry$getId(), id)

# Try to get an non-existing entry
entry <- conn$getGeneEntry(2139485387547754)
checkTrue(is.null(entry))

# Get another entry
id <- 7273
entry <- conn$getGeneEntry(id)
checkTrue( ! is.null(entry))
checkEquals(entry$getId(), id)
