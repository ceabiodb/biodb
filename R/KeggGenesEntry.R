# vi: fdm=marker

#' @include KeggEntry.R

# Class declaration {{{1
################################################################

KeggGenesEntry <- methods::setRefClass("KeggGenesEntry", contains = 'KeggEntry')

# Constructor {{{1
################################################################

KeggGenesEntry$methods( initialize = function(...) {

	callSuper(...)
})

# Parse fields step 2 {{{1
################################################################

KeggGenesEntry$methods( .parseFieldsStep2 = function(parsed.content) {

	# Name
	.self$.parseMultilinesField(field = 'name', tag = 'NAME', parsed.content = parsed.content, strip.chars = ' ;', split.char = ',')

	# Adjust accession with organism code
	if (.self$hasField('kegg.organism.code'))
		.self$setFieldValue('accession', paste(.self$getFieldValue('kegg.organism.code'), .self$getFieldValue('accession'), sep = ':'))

	# Module IDs
	.self$.parseModuleIds(parsed.content)

	# Parse Uniprot IDs
	if (.self$hasField('uniprot.id'))
		.self$setFieldValue('uniprot.id', strsplit(.self$getFieldValue('uniprot.id'), ' +', perl = TRUE)[[1]])

	# AA SEQ
	lines = .self$.getTagLines(tag = 'AASEQ', parsed.content = parsed.content)
	seq.length = as.integer(lines[[1]])
	sequence = paste(lines[2:length(lines)], collapse = '')
	if (seq.length != nchar(sequence))
		.self$message('CAUTION', paste('Length of AA sequence (', nchar(sequence), ') is different from the stated length (', seq.length, '). In entry ', .self$getFieldValue('accession'), '.'))
	.self$setFieldValue('aa.seq', sequence)
	.self$setFieldValue('aa.seq.length', seq.length)

	# NT SEQ
	lines = .self$.getTagLines(tag = 'NTSEQ', parsed.content = parsed.content)
	seq.length = as.integer(lines[[1]])
	sequence = paste(lines[2:length(lines)], collapse = '')
	if (seq.length != nchar(sequence))
		.self$message('CAUTION', paste('Length of NT sequence (', nchar(sequence), ') is different from the stated length (', seq.length, '). In entry ', .self$getFieldValue('accession'), '.'))
	.self$setFieldValue('nt.seq', sequence)
	.self$setFieldValue('nt.seq.length', seq.length)
})
