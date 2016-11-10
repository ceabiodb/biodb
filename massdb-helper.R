if(! exists ("compareSpectra")) {
	
	source('spec-dist.R')
	
	simplifySpectrum <- function(spec) {
		if(length(spec) == 0){
			return(NA_real_)
		}
		#print(spec)
		if (nrow(spec) == 0)
			return(NA_real_)
		if (ncol(spec) != 2) {
			spec[, BIODB.PEAK.MZ]
			mint <- BIODB.GROUP.INTENSITY %in% colnames(libspec)
			pint <- which(mint[1])
			if (length(pint) == 0)
				stop(
					"No intensity column founds, if there is more than 2 column, columns should be named",
					paste0(BIODB.GROUP.INTENSITY, collapse = ", ")
				)
			spec <- spec[, c(BIODB.PEAK.MZ, BIODB.GROUP.INTENSITY[pint[1]])]
			###Normalizing the intenities.
		}
		spec[, 2] <- as.numeric(spec[, 2]) * 100 / max(as.numeric(spec[, 2]))
		colnames(spec) <- c(BIODB.PEAK.MZ, BIODB.PEAK.RELATIVE.INTENSITY)
		spec
	}
	
	
	
	calcDistance <-
		function(spec1 ,
				 spec2,
				 npmin = 2,
				 fun = c("wcosine"),
				 params = list()) {
			#fun <- match.arg(fun)
			
			#SPec are always notmlized in pourcentage toa voir issues;
			spec1 <- simplifySpectrum(spec1)
			spec2 <- simplifySpectrum(spec2)
			if(is.na(spec1)||is.na(spec2)) return(list(matched=numeric(0),similarity=0))
			params$mz1 <- as.numeric(spec1[, BIODB.PEAK.MZ])
			params$mz2 <- as.numeric(spec2[, BIODB.PEAK.MZ])
			params$int1 <- as.numeric(spec1[, BIODB.PEAK.RELATIVE.INTENSITY])
			params$int2 <- as.numeric(spec2[, BIODB.PEAK.RELATIVE.INTENSITY])
			res <- do.call(fun, args = params)
			if (sum(res$matched != -1) < npmin)
				return(list(matched = res$matched, similarity = 0))
			list(matched = res$matched,
				 similarity = res$measure)
		}
	
	
	
	###The returned sim list is not ordered
	compareSpectra <-
		function(spec,
				 libspec,
				 npmin = 2,
				 fun = BIODB.MSMS.DIST.WCOSINE,
				 params = list(),
				 decreasing = TRUE) {
			#fun <- match.arg(fun)
			if (length(libspec) == 0) {
				return(NULL)
			}
			if (nrow(spec) == 0) {
				return(NULL)
			}
			
			####spec is directly normalized.
			vall <-
				sapply(
					libspec,
					calcDistance,
					spec1 = spec,
					params = params,
					fun = fun,
					simplify = FALSE
				)
			####the list is ordered with the chosen metric.
			sim <-
				vapply(vall,
					   '[[',
					   i = "similarity",
					   FUN.VALUE = ifelse(decreasing, 0, 1))
			osim <- order(sim, decreasing = decreasing)
			matched <- sapply(vall, '[[', i = "matched", simplify = FALSE)
			
			return(list(
				ord = osim,
				matched = matched,
				similarity = sim
			))
		}
}