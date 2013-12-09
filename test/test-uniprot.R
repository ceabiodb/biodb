#!/usr/bin/env R --slave -f
library(RUnit)
source('../UniProtConn.R', chdir = TRUE)

# Open connexion
conn <- UniProtConn$new(useragent = "fr.cea.test-uniprot ; pierrick.rogermele@cea.fr")

# Get a real entry
id <- 'Q75MT5'
entry <- conn$getEntry(id)
checkEquals(entry$getAccession(), id)
checkTrue(entry$getName() != '')
checkTrue(entry$getLength() > 0)
checkTrue(entry$getMass() > 0)
