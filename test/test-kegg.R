source('../KeggConn.R', chdir = TRUE)

#############
# CONSTANTS #
#############

KEGG.ENTRIES <- list(
                list(type = 'kegg', id = 'hsa:3627'),
                list(type = 'kegg', id = 'ec:1.1.1.54'),
                list(type = 'kegg', id = 'BLABLABLA', false = TRUE),
                list(type = 'kegg', id = 'cpd:C00751', chebiid = '15440')
                )

####################
# ONLINE TEST KEGG #
####################

online.test.kegg <- function() {
	online.test(KEGG.ENTRIES)
}
