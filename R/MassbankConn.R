# vi: fdm=marker

#' @include RemotedbConn.R
#' @include MassdbConn.R

# Class declaration {{{1
################################################################

MassbankConn <- methods::setRefClass("MassbankConn", contains = c("RemotedbConn", "MassdbConn"))

# Constructor {{{1
################################################################0

MassbankConn$methods( initialize = function(...) {

	callSuper(content.type = BIODB.TXT, ...)
})

# Get mz values {{{1
################################################################

MassbankConn$methods( .doGetMzValues = function(ms.mode, max.results, precursor, ms.level) {

	mz <- numeric(0)

	# Get list of spectra
	.self$message(MSG.DEBUG, paste('max.results=', max.results, sep = ''))
	spectra.ids <- .self$searchMzRange(10, 1000, ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level)
	print('********************************')
	print('MassbankConn::.doGetMzValues 01')
	print(spectra.ids)
	print('********************************')
	print('MassbankConn::.doGetMzValues 02')

	# Get entries
	entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), spectra.ids, drop = FALSE)
	print(class(entries))
	print('********************************')

	# Get peaks
	df <- .self$getBiodb()$entriesToDataframe(entries, only.atomic = FALSE)
	if (BIODB.PEAK.MZ %in% colnames(df))
		mz <- df[[BIODB.PEAK.MZ]]

	# Cut
	if ( ! is.na(max.results) && length(mz) > max.results)
		mz <- mz[1:max.results]

	return(mz)
})

# Do search M/Z with tolerance {{{1
################################################################

MassbankConn$methods( .doSearchMzTol = function(mz, tol, tol.unit, min.rel.int, ms.mode, max.results, precursor, ms.level) {

	returned.ids <- NULL

	# Set tolerance
	if (tol.unit == BIODB.MZTOLUNIT.PPM)
		tol <- tol * mz * 1e-6

	# Build request
	xml.request <- paste('<?xml version="1.0" encoding="UTF-8"?><SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:tns="http://api.massbank"><SOAP-ENV:Body><tns:searchPeak><tns:mzs>', mz, '</tns:mzs><tns:relativeIntensity>', if (is.na(min.rel.int)) 0 else min.rel.int, '</tns:relativeIntensity><tns:tolerance>', tol, '</tns:tolerance><tns:instrumentTypes>all</tns:instrumentTypes><tns:ionMode>', if (is.na(ms.mode)) 'Both' else ( if (ms.mode == BIODB.MSMODE.NEG) 'Negative' else 'Positive'),'</tns:ionMode><tns:maxNumResults>', if (is.na(max.results)) 0 else max.results, '</tns:maxNumResults></tns:searchPeak></SOAP-ENV:Body></SOAP-ENV:Envelope>', sep = '')

	# Send request
	xmlstr <- .self$.scheduler$sendSoapRequest(paste0(.self$getBaseUrl(), 'api/services/MassBankAPI.MassBankAPIHttpSoap11Endpoint/'), xml.request)

	# Parse XML and get text
	if ( ! is.na(xmlstr)) {
		xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)
		ns <- c(ax21 = "http://api.massbank/xsd")
		returned.ids <- XML::xpathSApply(xml, "//ax21:id", XML::xmlValue, namespaces = ns)

		# TODO Filter on precursor
		# TODO Filter on ms.level
	}

	return(returned.ids)
})

# Do search M/Z range {{{1
################################################################

MassbankConn$methods( .doSearchMzRange = function(mz.min, mz.max, min.rel.int, ms.mode, max.results, precursor, ms.level) {
	mz <- (mz.min + mz.max) / 2
	tol <- mz.max - mz
	return(.self$searchMzTol(mz = mz, tol = tol, tol.unit = BIODB.MZTOLUNIT.PLAIN, min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level))
})

# Do get entry content url {{{1
################################################################

MassbankConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {

	                  # TODO Return an URL request object with SOAP message embedded
	if (concatenate)
		url <- paste(.self$getBaseUrl(), 'getRecordInfo?ids=', paste(id, collapse = ','), sep = '')
	else
		url <- paste(.self$getBaseUrl(), 'getRecordInfo?ids=', id, sep = '')

	return(url)
})
