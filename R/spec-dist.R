trpz <- function (x, y) 
{
	if (missing(y)) {
		if (length(x) == 0) 
			return(0)
		y <- x
		x <- seq(along=x)
	}
	if (length(x) == 0 && length(y) == 0) 
		return(0)
	if (!(is.numeric(x) || is.complex(x)) || !(is.numeric(y) || 
											   is.complex(y))) 
		stop("Arguments 'x' and 'y' must be real or complex vectors.")
	m <- length(x)
	if (length(y) != m) 
		stop("Arguments 'x', 'y' must be vectors of the same length.")
	if (m <= 1) 
		return(0)
	xp <- c(x, x[m:1])
	yp <- c(numeric(m), y[m:1])
	n <- 2 * m
	p1 <- sum(xp[seq_len(n - 1)] * yp[2:n]) + xp[n] * yp[1]
	p2 <- sum(xp[2:n] * yp[seq_len(n - 1)]) + xp[1] * yp[n]
	return(0.5 * (p1 - p2))
}

matchPpm <- function(x, y, ppm=3, mzmin=0) {
	if (any(is.na(y)))
		stop("NA's are not allowed in y !\n")
	ok <- !(is.na(x))
	ans <- order(x)
	keep <- seq_along(ok)[ok]
	xidx <- ans[ans %in% keep]
	xs <- x[xidx]
	yidx <- order(y)
	ys <- y[yidx]
	if (!is.double(xs))
		xs <- as.double(xs)
	if (!is.double(ys))
		ys <- as.double(ys)
	if (!is.integer(xidx))
		xidx <- as.integer(xidx)
	if (!is.integer(yidx))
		yidx <- as.integer(yidx)
	
	fm <-
		.Call(
			"closeMatchPpm",
			xs,
			ys,
			xidx,
			yidx,
			as.integer(length(x)),
			as.double(ppm),
			as.double(mzmin)
		)
	fm
}


simpList <- function(v) {
	vapply(v, function(x) {
		if (is.null(x)) {
			-1
		} else{
			x
		}
	}, FUN.VALUE=-1)
}


##Stein and scott values : mzexp 3 intexp 0.6
##Massbank values : mzexp 2 intexp 0.5
cosine <-
	function(mz1,
			 mz2,
			 int1,
			 int2,
			 mzexp=2,
			 intexp=0.5,
			 ppm=3,
			 dmz=0.005) {
		matchList <- matchPpm(mz1, mz2, ppm, dmz)
		###Weigthed intensity
		pfound <- which(!vapply(matchList, is.null, FUN.VALUE=TRUE))
		
		###If no peak is found.
		if (length(pfound) == 0)
			return(list(measure=0, matched=rep(-1, length(mz1))))
		w1 <- int1 ^ intexp * mz1 ^ mzexp
		w2 <- int2 ^ intexp * mz2 ^ mzexp
		cos_value <-
			sum((w1[pfound] * w2[unlist(matchList[pfound])]) ^ 2) / (sum(w1[pfound] ^
																			2) * sum(w2[unlist(matchList[pfound])] ^ 2))
		
		####Adding the penality if needed.
		list(measure=cos_value, matched=simpList(matchList))
	}


###penalized cosine

wcosine <-
	function(mz1,
			 mz2,
			 int1,
			 int2,
			 mzexp=2,
			 intexp=0.5,
			 ppm=3,
			 dmz=0.005,
			 penality=c("rweigth")) {
		penality <- match.arg(penality)
		matchList <- matchPpm(mz1, mz2, ppm, dmz)
		###Weigthed intensity
		pfound <- which(!vapply(matchList, is.null, FUN.VALUE=TRUE))
		###If no peak is found.
		if (length(pfound) == 0)
			return(list(measure=0, matched=rep(-1, length(mz1))))
		w1 <- int1 ^ intexp * mz1 ^ mzexp
		w2 <- int2 ^ intexp * mz2 ^ mzexp
		
		cos_value <-
			sum((w1[pfound] * w2[unlist(matchList[pfound])]) ^ 2) / (sum(w1[pfound] ^
																			2) * sum(w2[unlist(matchList[pfound])] ^ 2))
		
		if (is.nan(cos_value))
			cos_value <- 0
		####Adding the penality if needed.
		div=1
		if (penality == "rweigth") {
			p <-
				(sum(w1[pfound]) / sum(w1) + sum(w2[unlist(matchList[pfound])]) / sum(w2)) /
				2
			div=2
		} else{
			p <- 0
		}
		
		measure <-  (cos_value + p) / div
		if (is.nan(measure))
			measure <-  (cos_value) / div
		list(measure=measure,
			 matched=simpList(matchList))
	}

##The spec is seen as the mixture of two gaussian.
pkernel <-
	function(mz1,
			 mz2,
			 int1,
			 int2,
			 mzexp=2,
			 intexp=0.5,
			 ppm=3,
			 ###here ppm is the sigma ofthe mass distribution.
			 dmz=0.005,
			 penality=c("rweigth")) {
		###We first match the peak
		matchList <- matchPpm(mz1, mz2, ppm, dmz)
		# ###Weigthed intensity
		pfound <- which(!vapply(matchList, is.null, FUN.VALUE=TRUE))
		#
		###If no peak is found.
		if (length(pfound) == 0)
			return(list(measure=0, matched=rep(-1, length(mz1))))
		w1 <- int1 ^ intexp * mz1 ^ mzexp
		w2 <- int2 ^ intexp * mz2 ^ mzexp
		
		#w1 and w2 are normalized.
		w1 <- w1 * 1 / sum(w1)
		w2 <- w2 * 1 / sum(w2)
		
		l1 <- length(w1)
		l2 <- length(w2)
		###The mz dev
		vsig1=mz1 * ppm * 3 * 10 ^ -6
		vsig1=vapply(vsig1, function(x, y) max(x, y), y=dmz, FUN.VALUE=1.0)
		
		vsig2=mz2 * ppm * 3 * 10 ^ -6
		vsig2=vapply(vsig2, function(x, y) max(x, y), y=dmz, FUN.VALUE=1.0)
		accu=0
		###TO DO rcopder en C
		for (i in seq_len(l1)) {
			for (j in seq_len(l2)) {
				divisor=max(
					stats::dnorm(
						mz1[i],
						mean=mz1[i],
						sd=sqrt(vsig1[i] ^ 2 + vsig1[i] ^ 2)
					),
					stats::dnorm(
						mz2[j],
						mean=mz2[j],
						sd=sqrt(vsig2[j] ^ 2 + vsig2[j] ^ 2)
					)
				)
				if (divisor == 0)
					next
				scalet=stats::dnorm(mz1[i],
									  mean=mz2[j],
									  sd=sqrt(vsig1[i] ^ 2 + vsig2[j] ^ 2))
				accu=accu + scalet / divisor
			}
		}
		div=1
		if (penality == "rweigth") {
			p <-
				(sum(w1[pfound]) / sum(w1) + sum(w2[unlist(matchList[pfound])]) / sum(w2)) /
				2
			div=2
		} else{
			p <- 0
		}
		accu=accu / (l2 * l1)
		list(measure=(accu + p) / div,
			 matched=simpList(matchList))
	}

###Uing the bachttarya similarity :
pbachtttarya <-
	function(mz1,
			 mz2,
			 int1,
			 int2,
			 mzexp=2,
			 intexp=0.5,
			 ppm=3,
			 ###here ppm is the sigma ofthe mass distribution.
			 dmz=0.005,
			 penality=c("rweigth")) {
		###We first match the peak
		matchList <- matchPpm(mz1, mz2, ppm, dmz)
		# ###Weigthed intensity
		pfound <- which(!vapply(matchList, is.null, FUN.VALUE=TRUE))
		#
		###If no peak is found.
		if (length(pfound) == 0)
			return(list(measure=0, matched=rep(-1, length(mz1))))
		w1 <- int1 ^ intexp * mz1 ^ mzexp
		w2 <- int2 ^ intexp * mz2 ^ mzexp
		
		#w1 and w2 are normalized.
		w1 <- w1 * 1 / sum(w1)
		w2 <- w2 * 1 / sum(w2)
		
		l1 <- length(w1)
		l2 <- length(w2)
		###The mz dev
		vsig1=mz1 * ppm * 3 * 10 ^ -6
		vsig1=vapply(vsig1, function(x, y) max(x, y), y=dmz, FUN.VALUE=1.0)
		
		vsig2=mz2 * ppm * 3 * 10 ^ -6
		vsig2=vapply(vsig2, function(x, y) max(x, y), y=dmz, FUN.VALUE=1.0)
		
		accu=0
		
		###For each matched peak in we compute the Bhattacharyya coefficient:
		for (j in seq_len(length(matchList))) {
			if (is.null(matchList[[j]]))
				next
			mz1v <- mz1[j]
			mz2v <- mz2[matchList[[j]]]
			sig1 <- vsig1[j]
			sig2 <- vsig2[matchList[[j]]]
			#cat(paste("ml",matchList[[j]],"mz1 ",mz1," mz2 ",mz2," sig1 ",sig1," sig2 ",sig2,"\n"))
			sig <- max(sig1, sig2)
			xseq <- seq(min(mz1v, mz2v) - 4 * sig, max(mz1v, mz2v) + 4 * sig, length =
							100)
			y1 <- stats::dnorm(xseq, mean=mz1v, sd=sig)
			y2 <- stats::dnorm(xseq, mean=mz2v, sd=sig)
			accu=accu + sum(trpz(xseq,sqrt(y1 * y2)))
		}
		div=1
		if (penality == "rweigth") {
			p <-
				(sum(w1[pfound]) / sum(w1) + sum(w2[unlist(matchList[pfound])]) / sum(w2)) /
				2
			div=2
		} else{
			p <- 0
		}
		accu=accu / (l2 * l1)
		list(measure=(accu + p) / div,
			 matched=simpList(matchList))
	}
