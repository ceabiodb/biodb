# vi: fdm=marker

#' @include BiodbJsonEntry.R

# Class declaration {{{1
################################################################

PeakforestMassEntry <- methods::setRefClass("PeakforestMassEntry", contains = "BiodbJsonEntry")

# Constructor {{{1
################################################################

PeakforestMassEntry$methods( initialize = function(...) {
	callSuper(...)
})

# Parse fields step 2 {{{1
################################################################

PeakforestMassEntry$methods( .parseFieldsStep2 = function(parsed.content) {

	# Set peaks
	if ('peaks' %in% names(parsed.content) && length(parsed.content$peaks) > 0) {

		# Creaate empty peaks data frame
		peaks <- data.frame(mz = double(), ri = double(), deltaPPM = double(), theoricalMass = double(), composition = character(), attribution = character(), stringsAsFactors = FALSE)

		# Loop on all peaks
		for (p in parsed.content$peaks) {

			# Set empty values to NA
			peak <- list()
			for (field in colnames(peaks))
				if (field %in% names(p)) {
					if (length(p[[field]]) > 0)
						peak[[field]] <- p[[field]]
					else
						peak[[field]] <- as.vector(NA, mode = class(peaks[[field]]))
				}
			peak <- data.frame(peak, stringsAsFactors = FALSE)

			# Append peak to peaks data frame
			peaks <- rbind(peaks, peak)
		}

		# Set right column names
		colnames(peaks) <- c('peak.mz', 'peak.relative.intensity', 'peak.error.ppm', 'peak.mass', 'peak.comp', 'peak.attr')
		.self$setFieldValue('peaks', peaks)
		.self$setFieldValue('nb.peaks', nrow(peaks))
	}

	# Set retention time unit
	if (.self$hasField('chrom.rt.min') || .self$hasField('chrom.rt.max'))
		.self$setField('chrom.rt.unit', 'min')

	# Parse compound IDs
	if ('listOfCompounds' %in% names(parsed.content)) {
		for (c in parsed.content$listOfCompounds)
			.self$appendFieldValue('peakforest.compound.id', c$id)

		# In case only one compound is listed, parse all compound fields.
		if (length(parsed.content$listOfCompounds) == 1) {
			comp <- parsed.content$listOfCompounds[[1]]
			fields <- list('inchikey' = 'inChIKey',
			               'inchi' = 'inChI',
			               'ncbi.pubchem.comp.id' = 'PubChemCID',
			               'kegg.compound.id' = 'KEGG',
			               'chebi.id' = 'ChEBI',
			               'hmdb.metabolites.id' = 'HMDB',
			               'formula' = 'formula',
			               'monoisotopic.mass' = 'monoisotopicMass',
			               'average.mass' = 'averageMass',
			               'smiles' = 'canSmiles',
			               'logp' = 'logP',
			               'name' = 'mainName')
			for (f in names(fields)) {
				comp.f <- fields[[f]]
				if (comp.f %in% names(comp)) {
					v <- comp[[comp.f]]
					if (f == 'hmdb.metabolites.id' && v == 'HMDBnull')
						v <- NULL
					if ( ! is.null(v) && ! is.list(v)) {
						if (f == 'chebi.id')
							v <- sub('^CHEBI:', '', v)
						.self$setFieldValue(f, v)
					}
				}
			}
		}
	}

	# Set MS level
	if ('fragmentationLevelString' %in% names(parsed.content)) {
		if (parsed.content$fragmentationLevelString == 'MS2')
			.self$setFieldValue('ms.level', 2)
		else
			.self$message('caution', paste('Unknown MS type "', parsed.content$fragmentationLevelString,'" for Peakforest entry "', .self$getFieldValue('accession'), '".', sep = ''))
	}
	else
		.self$setFieldValue('ms.level', 1)

	# Get precursor
	if ( ! .self$hasField('msprecmz') && .self$hasField('peaks')) {
		prec <- .self$getFieldValue('peaks')[['peak.attr']] %in% c('[M+H]+', '[M-H]-')
		if (any(prec))
			.self$setFieldValue('msprecmz', .self$getFieldValue('peaks')[prec, 'peak.mz'])
	}
})
