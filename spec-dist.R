	

###Symetric visibly do the same thing than I do.
matchPpm <- function(x,y,ppm=3, mzmin=0) {
  
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
    ys<- as.double(ys)
  if (!is.integer(xidx))
    xidx <- as.integer(xidx)
  if (!is.integer(yidx))
    yidx <- as.integer(yidx)
  
  fm <- .Call("closeMatchPpm", xs, ys, xidx, yidx, as.integer(length(x)), as.double(ppm),as.double(mzmin))
  fm
}


simpList <- function(v){
    vapply(v,function(x){if(is.null(x)){-1}else{x}},FUN.VALUE = -1)
}


##Stein and scott values : mzexp 3 intexp 0.6
##Massbank values : mzexp 2 intexp 0.5


cosine<-function(mz1,mz2,int1,int2,mzexp=2,intexp=0.5,ppm,dmz=0.005){
    matchList <- matchPpm(mz1,mz2,ppm,dmz)
    ###Weigthed intensity
    pfound <- which(!sapply(matchList,is.null,simplify=TRUE))
    
    ###If no peak is found.
    if(length(pfound)==0) return(list(measure=0,matched=rep(-1,length(mz1))))
    w1 <- int1^intexp*mz1^mzexp
    w2 <- int2^intexp*mz2^mzexp
    cos_value <- sum((w1[pfound]*w2[unlist(matchList[pfound])])^2)/(sum(w1[pfound]^2)*sum(w2[unlist(matchList[pfound])]^2))

    ####Adding the penality if needed.
    list(measure = cos_value, matched = simpList(matchList))
}


###penalized cosine

wcosine<-function(mz1,mz2,int1,int2,mzexp=2,intexp=0.5,ppm,dmz=0.005,penality=c("rweigth")){
    penality <- match.arg(penality)
    matchList <- matchPpm(mz1,mz2,ppm,dmz)
    ###Weigthed intensity
    pfound <- which(!sapply(matchList,is.null,simplify=TRUE))
    
    ###If no peak is found.
    if(length(pfound)==0) return(list(measure=0,matched=rep(-1,length(mz1))))
    w1 <- int1^intexp*mz1^mzexp
    w2 <- int2^intexp*mz2^mzexp

    cos_value <- sum((w1[pfound]*w2[unlist(matchList[pfound])])^2)/(sum(w1[pfound]^2)*sum(w2[unlist(matchList[pfound])]^2))
    
    ####Adding the penality if needed.
    div = 1
    if(penality == "rweigth"){
    	p <- (sum(w1[pfound])/sum(w1) + sum(w2[unlist(matchList[pfound])])/sum(w2))/2
    	div = 2
    }else{
    	p <- 0
    }
    list(measure = (cos_value+p)/div, matched = simpList(matchList))
}

##A gaussian of the two spectra seen as a mixture of gaussian, derived form Heinonen et al 2012
pkernel <- function(mz1,mz2,int1,int2,mzexp=2,intexp=0.5,ppm,dmz=0.005, sigint =0.5,penality=c("rweigth")){
	###We first match the peak
	# matchList <- matchPpm(mz1,mz2,ppm,dmz)
	# ###Weigthed intensity
	# pfound <- which(!sapply(matchList,is.null,simplify=TRUE))
	# 
	###If no peak is found.
	#if(length(pfound)==0) return(list(measure=0,matched=rep(-1,length(mz1))))
	w1 <- int1^intexp*mz1^mzexp
	w2 <- int2^intexp*mz2^mzexp
	
	w1 <- w1*1/sum(w1)
	w2 <- w2*1/sum(w2)
	
	
	l1 <- length(w1)
	l2 <- length(w2)
	
	###The mz dev
	vsig1=mz1*ppm*3*10^-6
	vsig1 = sapply(vsig1,function(x,y){
		return(max(x,y))
	},y=dmz)
	
	vsig2=mz2*ppm*3*10^-6
	vsig2 = sapply(vsig2,function(x,y){
		return(max(x,y))
	},y=dmz)
	
	accu=0
	###TO DO rcopder en C
	for(i in 1:l1){
		for(j in 1:l2){
			divisor = max(dnorm(mz1[i],mean=mz1[i],sd=sqrt(vsig1[i]^2+vsig1[i]^2)),
						  dnorm(mz2[j],mean=mz2[j],sd=sqrt(vsig2[j]^2+vsig2[j]^2)))
			if(divisor==0) next
			scalet= dnorm(mz1[i],mean=mz2[j],sd=sqrt(vsig1[i]^2+vsig2[j]^2))
			accu=accu+scalet/divisor

		}
	}
	div = 1
	if(penality == "rweigth"){
		p <- (sum(w1[pfound])/sum(w1) + sum(w2[unlist(matchList[pfound])])/sum(w2))/2
	    div = 2
	}else{
		p <- 0
	}
	
	accu=accu/(l2*l1)
	list(measure = (accu+p)/div, matched = simpList(matchList))
}