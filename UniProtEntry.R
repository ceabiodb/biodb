library(XML)
source('BioDbEntry.R')

#####################
# CLASS DECLARATION #
#####################

UniProtEntry <- setRefClass("UniProtEntry", contains = "BioDbEntry", fields = list(enzyme_id = "character", sequence = "character", mass = "numeric", length = "numeric", gene_symbol = "character", fullname = "character", name = "character"))

###############
# CONSTRUCTOR #
###############

UniProtEntry$methods(
	initialize = function(enzyme_id = NA_character_, sequence = NA_character_, mass = NA_real_, length = NA_integer_, gene_symbol = NA_character_, fullname = NA_character_, name = NA_character_, ...) {
		enzyme_id <<- if ( ! is.null(enzyme_id)) enzyme_id else NA_character_
		sequence <<- if ( ! is.null(sequence)) sequence else NA_character_
		mass <<- if ( ! is.null(mass)) mass else NA_real_
		length <<- if ( ! is.null(length)) length else NA_integer_
		gene_symbol <<- if ( ! is.null(gene_symbol)) gene_symbol else NA_character_
		fullname <<- if ( ! is.null(fullname)) fullname else NA_character_
		name <<- if ( ! is.null(name)) name else NA_character_

		callSuper(...)
	}
)

########
# NAME #
########

UniProtEntry$methods(
	getName = function() {
		return(.self$name)
	}
)

############
# FULLNAME #
############

UniProtEntry$methods(
	getFullName = function() {
		return(.self$fullname)
	}
)

###############
# GENE SYMBOL #
###############

UniProtEntry$methods(
	getGeneSymbol = function() {
		return(.self$gene_symbol)
	}
)

##########
# LENGTH #
##########

UniProtEntry$methods(
	getLength = function() {
		return(.self$length)
	}
)

########
# MASS #
########

UniProtEntry$methods(
	getMass = function() {
		return(.self$mass)
	}
)

############
# SEQUENCE #
############

UniProtEntry$methods(
	getSequence = function() {
		return(.self$sequence)
	}
)

#############
# ENZIME ID #
#############

# <dbReference type="EC" id="2.3.1.43"/>
UniProtEntry$methods(
	getEnzymeId = function() {
		return(.self$enzyme_id)
	}
)

###########
# FACTORY #
###########

createUniProtEntryFromXml <- function(xmlstr) {

	# If the entity doesn't exist (i.e.: no <id>.xml page), then it returns an HTML page
	if (grepl("^<!DOCTYPE html PUBLIC", xmlstr, perl=TRUE))
		return(NULL)

	# Set XML namespace
	ns <- c(uniprot = "http://uniprot.org/uniprot")

	# Parse XML
	xml <-  xmlInternalTreeParse(xmlstr, asText = TRUE)

	# Get data
	accession_ids <- unlist(xpathSApply(xml, "//uniprot:accession", xmlValue, namespaces = ns))
	id <- if ( ! is.null(accession_ids) && length(accession_ids) > 1) accession_ids[1] else accession_ids
	kegg_id <- unlist(xpathSApply(xml, "//uniprot:dbReference[@type='KEGG']", xmlGetAttr, 'id', namespaces = ns))
	enzyme_id <- unlist(xpathSApply(xml, "//uniprot:dbReference[@type='EC']", xmlGetAttr, 'id', namespaces = ns))
	seq <- unlist(xpathSApply(xml, "//uniprot:entry/uniprot:sequence", xmlValue, namespaces = ns))
	if ( ! is.null(seq) && ! is.na(seq)) seq <- gsub("\\n", "", seq)
	mass <- as.numeric(unlist(xpathSApply(xml, "//uniprot:entry/uniprot:sequence", xmlGetAttr, 'mass', namespaces = ns)))
	length <- as.numeric(unlist(xpathSApply(xml, "//uniprot:entry/uniprot:sequence", xmlGetAttr, 'length', namespaces = ns)))
	gene_symbols <- unlist(xpathSApply(xml, "//uniprot:gene/uniprot:name", xmlValue, namespaces = ns))
	gene_symbol <- if ( ! is.null(gene_symbols) && length(gene_symbols) > 1) gene_symbols[1] else gene_symbols
	fullnames <- unlist(xpathSApply(xml, "//uniprot:protein//uniprot:fullName", xmlValue, namespaces = ns))
	fullname <- if ( ! is.null(fullnames) && length(fullnames) > 1) fullnames[1] else fullnames
	name <- unlist(xpathSApply(xml, "/uniprot:uniprot/uniprot:entry/uniprot:name", xmlValue, namespaces = ns))

	return(if (is.null(id) || is.na(id)) NULL else UniProtEntry$new(id = id, kegg_id = kegg_id, enzyme_id = enzyme_id, sequence = seq, mass = mass, length = length, gene_symbol = gene_symbol, fullname = fullname, name = name))
}
