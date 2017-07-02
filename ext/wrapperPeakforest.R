###Script wrapper pour biodb
args <- commandArgs(trailingOnly = F)
script.path <- sub("--file=","",args[grep("--file=",args)])

library(batch)
library(biodb)

###PARAMETERS :
# file : The path to the file containing the query spectra.
# example of spectrum :
# m/z int. rel.int.
# 310.05995 0.26 0.26
# 254.098 0.32 0.32
# 252.1009 1.2 1.2
# 263.13292 0.35 0.35
# 320.08807 0.78 0.78
# 208.1113 0.05 0.05
# 331.11925 0.02 0.02
# 321.09038 0.03 0.03
# 330.11694 1.3 1.3
# 322.08591 0.19 0.19
# 172.13425 0.2 0.2
# 216.12395 0.19 0.19
# 262.12984 3.35 3.35
# 312.05596 0.2 0.2
# 253.10365 0.08 0.08
# precursor : The m/z of the precursor to be matched.
# mztol : The tolerance in m/z for the precursor search.
# ppm : The ppm tolerance for the matching.
# mode : The polarity of the spectrza mode or neg

####EXAMPLE :
# Rscript wrapperPeakforest.R -file C:/Users/AD244905/Documents/tspec.txt -precursor 254 -mztol 0.1 -ppm 3 -mode neg
#
#

###STATIC PARAMETER TO BE DETERMINED INTERNALLY
# USER_AGENT : the user agent.
# NPMIN : The minimal number of matched peak to determine the parameters.
# DMZ : The minimal tolerance in ppm to account lower precision at low masses.
# MZEXP : The mz exponent.
# INTEXP : The int exponent.
# DIST : The matching function used.

NPMIN <- 2
DMZ <- 0.005
MZEXP <- 2
INTEXP <- 0.5
DIST <- 'pbachtttarya'

searchMSMSPeakforest <- function(spec, precursor, mztol, ppm, mode) {

	###Putting the data.frame in form
	spec <- spec[,c(1,2)]
	colnames(spec) <- c("mz","intensity")
	
	biodb_obj <-  biodb:::Biodb$new()
	fac <- biodb_obj$getFactory()
	pfcon <- fac$createConn(biodb:::BIODB.PEAKFOREST.LCMS)
	
	res <- pfcon$msmsSearch(spec = spec, precursor = precursor, npmin = NPMIN, mztol=mztol, mode=mode,tolunit=BIODB.MZTOLUNIT.PLAIN,fun = DIST,params = list(ppm = ppm, dmz = DMZ, mzexp = MZEXP, intexp = INTEXP))
	return(res)
}

####Parsing the command line arguments :

listArguments = parseCommandArgs(evaluate=FALSE)

if(is.null(listArguments[['-file']])) stop("file should be a character giving access to pointing to csv file with mass as first columns and intensity as second columns.")

if(!file.exists(listArguments[['-file']])) stop(paste("file",listArguments[['-file']],"not found."))
spec <- read.table(listArguments[['-file']],sep=" ",header=TRUE)

if(is.null(listArguments[['-precursor']])) stop("precursor should be a real correspoding to the precursor mass.")
prec <- as.numeric(listArguments[['-precursor']])

if(is.null(listArguments[['-mztol']])) stop("mztol is the interval in which the spectra will be matched.")
mztol <- as.numeric(listArguments[['-mztol']])

if(is.null(listArguments[['-ppm']])) stop("ppm is the ppm parameters for the matching of the peaks.")
ppm <- listArguments[['-ppm']]

mode <- NULL
if(is.null(listArguments[['-mode']])) mode <- 'pos'
mode <- listArguments[['-mode']]

res <- searchMSMSPeakforest(spec,prec,mztol,ppm,mode)
print(jsonlite::toJSON(res))
