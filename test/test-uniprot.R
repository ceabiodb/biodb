#!/usr/bin/env R --slave -f
source('../UniProtConn.R', chdir = TRUE)
conn <- UniProtConn$new(useragent = "fr.cea.test-uniprot ; pierrick.rogermele@cea.fr")
entry <- conn$getEntry('Q75MT5')
entry$getAccession()
entry$getName()
entry$getLength()
entry$getMass()
