#!/usr/bin/env R --slave -f
source('../UniProtConn.R')
conn <- UniProtConn$new(useragent = "fr.cea.test-uniprot ; pierrick.rogermele@cea.fr")
conn$getEntry('Q75MT5')
