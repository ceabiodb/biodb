#############
# DATABASES #
#############

# DEPRECATED Mode values
BIODB.MSMODE.NEG <- 'neg'
BIODB.MSMODE.POS <- 'pos'

# Tolerance values
BIODB.TOL <- 'mztol'
BIODB.MZTOLUNIT.PPM <- 'ppm'
BIODB.MZTOLUNIT.PLAIN <- 'plain' # same as mz: mass-to-charge ratio
BIODB.MZTOLUNIT.VALS <- c(BIODB.MZTOLUNIT.PPM, BIODB.MZTOLUNIT.PLAIN)

########################
# MS-MS MEASURE VALUES #
########################

BIODB.MSMS.DIST.COS <- "cosine"
BIODB.MSMS.DIST.WCOSINE <- "wcosine"
BIODB.MSMS.DIST.PKERNEL <- "pkernel"
BIODB.MSMS.DIST.PBACH <- "bachtttarya"
