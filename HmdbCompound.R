source('BiodbCompound.R')

#####################
# CLASS DECLARATION #
#####################

HmdbCompound <- setRefClass("HmdbCompound", contains = "BiodbCompound", fields = list(name = "character", formula = "character", super_class = "character", average_mass = "numeric", monoisotopic_mass = "numeric"))

###############
# CONSTRUCTOR #
###############

HmdbCompound$methods(
	initialize = function(name = NA_character_, formula = NA_character_, super_class = NA_character_, average_mass = NA_real_, monoisotopic_mass = NA_real_, ...) {
		name <<- if ( ! is.null(name)) name else NA_character_
		formula <<- if ( ! is.null(formula)) formula else NA_character_
		super_class <<- if ( ! is.null(super_class)) super_class else NA_character_
		average_mass <<- if ( ! is.null(average_mass)) average_mass else NA_character_
		monoisotopic_mass <<- if ( ! is.null(monoisotopic_mass)) monoisotopic_mass else NA_character_
		callSuper(...)
})

########
# NAME #
########

HmdbCompound$methods(
	getName = function() {
		return(.self$name)
	}
)

####################
# CHEMICAL FORMULA #
####################

HmdbCompound$methods(
	getFormula = function() {
		return(.self$formula)
	}
)

###############
# SUPER CLASS #
###############

HmdbCompound$methods(
	getSuperClass = function() {
		return(.self$super_class)
	}
)

################
# AVERAGE MASS #
################

HmdbCompound$methods(
	getAverageMass = function() {
		return(.self$average_mass)
	}
)

#####################
# MONOISOTOPIC MASS #
#####################

HmdbCompound$methods(
	getMonoisotopicMass = function() {
		return(.self$monoisotopic_mass)
	}
)

###########
# FACTORY #
###########

createHmdbCompoundFromXml <- function(xmlstr) {

	library(XML)

	# Parse XML
	xml <-  xmlInternalTreeParse(xmlstr, asText = TRUE)

	# An error occured
	if (length(getNodeSet(xml, "//error")) != 0)
		return(NULL)

	# Get data
	id      <- xpathSApply(xml, "/metabolite/accession", xmlValue)
	kegg_id <- xpathSApply(xml, "//kegg_id", xmlValue)
	name    <- xpathSApply(xml, "/metabolite/name", xmlValue)
	formula <- xpathSApply(xml, "/metabolite/chemical_formula", xmlValue)
	super_class     <- xpathSApply(xml, "//super_class", xmlValue)
	average_mass    <- as.numeric(xpathSApply(xml, "//average_molecular_weight", xmlValue))
	monoisotopic_mass   <- as.numeric(xpathSApply(xml, "//monisotopic_moleculate_weight", xmlValue))

	return(if (is.na(id)) NULL else HmdbCompound$new(id = id, kegg_id = kegg_id, name = name, formula = formula, super_class = super_class, average_mass = average_mass, monoisotopic_mass = monoisotopic_mass))
}
