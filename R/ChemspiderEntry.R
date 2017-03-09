# vi: fdm=marker

# Class declaration {{{1
################################################################

ChemspiderEntry <- methods::setRefClass("ChemspiderEntry", contains = "BiodbEntry")

# Parse content {{{1
################################################################

ChemspiderEntry$methods( parseContent = function(content) {

	# Define xpath expressions
	xpath.expr <- character()
	xpath.expr[[BIODB.ACCESSION]]    	<- "//CSID"
	xpath.expr[[BIODB.FORMULA]]      	<- "//MF"
	xpath.expr[[BIODB.NAME]]         	<- "//CommonName"
	xpath.expr[[BIODB.AVERAGE.MASS]] 	<- "//AverageMass"
	xpath.expr[[BIODB.MONOISOTOPIC.MASS]] 	<- "//MonoisotopicMass"
	xpath.expr[[BIODB.NOMINAL.MASS]] 	<- "//NominalMass"
	xpath.expr[[BIODB.MONOISOTOPIC.MASS]] 	<- "//MonoisotopicMass"
	xpath.expr[[BIODB.MOLECULAR.WEIGHT]] 	<- "//MolecularWeight"
	xpath.expr[[BIODB.INCHI]]           <- "//InChI"
	xpath.expr[[BIODB.INCHIKEY]]       	<- "//InChIKey"
	xpath.expr[[BIODB.SMILES]]          <- "//SMILES"

	# Parse XML
	xml <-  XML::xmlInternalTreeParse(content, asText = TRUE)

	# Test generic xpath expressions
	for (field in names(xpath.expr)) {
		v <- XML::xpathSApply(xml, xpath.expr[[field]], XML::xmlValue)
		if (length(v) > 0)
			.self$setFieldValue(field, v)
	}
})
