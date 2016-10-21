if( ! exists('matchPpm')){
	
dyn.load('src/closeMatchPpm.dll')

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

wcosine<-function(mz1,mz2,int1,int2,mzexp=2,intexp=0.5,ppm,dmz=0.005,penality=c("rweigth","rintensity","none")){
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
    if(penality == "rweigth"){
        p <- (sum(w1[pfound])/sum(w1) + sum(w2[unlist(matchList[pfound])])/sum(w2))/2
    }else{
        p <- 0
    }
    list(measure = (cos_value+p)/2, matched = simpList(matchList))
}
}