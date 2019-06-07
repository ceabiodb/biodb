# Simplify spectrum {{{1
################################################################################

simplifySpectrum <- function(spec) {
	if(length(spec) == 0){
		return(NA_real_)
	}
	if (nrow(spec) == 0)
		return(NA_real_)

	# Get mz vals
	mz.vals <- NULL
	for (mzcol in c('mz', 'peak.mz', 'peak.mztheo', 'peak.mzexp'))
		if (mzcol %in% colnames(spec)) {
			mz.vals <- spec[, mzcol]
			break
		}
	if (is.null(mz.vals))
		stop("Cannot find MZ values.")

	int.vals <- NULL
	for (int.col in c('rel.int', 'peak.relative.intensity'))
		if (int.col %in% colnames(spec)) {
			int.vals <- spec[, int.col]
			break
		}
	if (is.null(int.vals)) {
		for (int.col in c('int', 'peak.intensity'))
			if (int.col %in% colnames(spec)) {
				int.vals <- spec[, int.col,]
				int.vals <- int.vals * 100 / max(int.vals)
				break
			}
	}
	if (is.null(int.vals))
		stop("Cannot find intensity values.")

	spec <- data.frame(mz=mz.vals, int=int.vals)
	colnames(spec) <- c('peak.mz', 'peak.relative.intensity')
	return(spec)
}

# Calc distance {{{1
################################################################################

calcDistance <-
	function(spec1 ,
			 spec2,
			 npmin=2,
			 fun=c("wcosine"),
			 params=list()) {
		#fun <- match.arg(fun)
		
		#SPec are always notmlized in pourcentage toa voir issues;
		spec1 <- simplifySpectrum(spec1)
		spec2 <- simplifySpectrum(spec2)
		if(any(is.na(spec1)) || any(is.na(spec2)))
			return(list(matched=numeric(0),similarity=0))
		params$mz1 <- as.numeric(spec1[, 'peak.mz'])
		params$mz2 <- as.numeric(spec2[, 'peak.mz'])
		params$int1 <- as.numeric(spec1[, 'peak.relative.intensity'])
		params$int2 <- as.numeric(spec2[, 'peak.relative.intensity'])
		res <- do.call(fun, args=params)
		if (sum(res$matched != -1) < npmin)
			return(list(matched=res$matched, similarity=0))
		list(matched=res$matched,
			 similarity=res$measure)
	}

# Compare spectra {{{1
################################################################################

###The returned sim list is not ordered
compareSpectra <- function(spec, libspec, npmin=2, fun="wcosine", params=list()) {

	res <- data.frame(score=numeric(0))

	# Add for peaks
	if ( ! is.null(spec)) {
		peak.cols <- paste('peak', seq(nrow(spec)), sep='.')
		for (p in peak.cols)
			res[[p]] <- integer(0)
	}

	if ( ! is.null(libspec) && ! is.null(spec) && length(libspec) > 0 && nrow(spec) > 0) {

		####spec is directly normalized.
		vall <- lapply(libspec, calcDistance, spec1=spec, npmin=npmin, params=params, fun=fun)

		####the list is ordered with the chosen metric.
		sim <- vapply(vall,	'[[', i="similarity", FUN.VALUE=1)
		matched <- lapply(vall, '[[', i="matched")
		
		res[seq_len(length(sim)), 'score'] <- sim
		for (i in seq_len(length(matched)))
			res[i, peak.cols] <- matched[[i]]

	}

	return(res)
}
