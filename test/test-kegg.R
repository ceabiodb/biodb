#############
# CONSTANTS #
#############

KEGG.ENTRIES <- list(
                list(type = RBIODB.KEGG, id = 'hsa:3627'),
                list(type = RBIODB.KEGG, id = 'ec:1.1.1.54'),
                list(type = RBIODB.KEGG, id = 'BLABLABLA', false = TRUE),
                list(type = RBIODB.KEGG, id = 'cpd:C00751', chebiid = '15440', inchi = 'InChI=1S/C30H50/c1-25(2)15-11-19-29(7)23-13-21-27(5)17-9-10-18-28(6)22-14-24-30(8)20-12-16-26(3)4/h15-18,23-24H,9-14,19-22H2,1-8H3/b27-17+,28-18+,29-23+,30-24+', inchikey = 'YYGNTYWPHWGJRM-AAJYLUCBSA-N')
                )

####################
# ONLINE TEST KEGG #
####################

online.test.kegg <- function() {
	online.test(KEGG.ENTRIES)
}
