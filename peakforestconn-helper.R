if( ! exists("searchMSMSPeakforest.R")){
source('PeakforestConn.R')

#' MSMS search using modified cosine similarity.
#' 
#' \code{msmsSearch} Performs a MSMS matching agaisnt peakforest, using a modified cosine similarity measure.
#' 
#' This function take as input a data.frame with 2 column, and a precursor match. The spectra are retrieved by matching
#' precursor in the given tolerance and the given polarity (optional) and return a list with 3 elements, the order 
#' peakforest ids of the spectra, the similarity values, and the matched peaks on the similarity spectra.
#' @param precursor The m/z value of the precursor.
#' @param mztol The spec to be compared in the precursor window.
#' @param tolunit mz unit for mztol, may be "ppm" or "plain".
#' @param sec The spectrum furnished as a two column data.frame. Intensities may be relative or plain.
#' @param ppm Ppm parameter to be used to perform the matching betgween fragments peaks.
#' @param mzmin The minimum value of the mz for matching, to overcome the lower paccuraccy of mass spectra
#' in low masses.
#' @param npmin The minimum number of peaks to be found for a spectrum to be matched.
#' @param useragent The useragent chain to be passed to Rcurl.
#' @param token The token to be passed to peakforest for the acquisition.
#' @return A list order by decreasing similarity containig three elements \enumerate{
#' \item A list of the Peakforest id of the matched spectra.
#' \item matchedpeaks A list containg numeric of the same length thans the input spec, giving for
#' each peak the number fo the peak which have been matched.
#' \item measure The similarity values calculated.
#' }
#' @examples
#' 
#' list(measure = res$similarity[ res$ord ], matchedpeaks = res$matched [ res$ord ], id = lret))

	searchMSMSPeakforest <- function(precursor, mztol, tolunit, spec, ppm , mzmin, token, npmin = 2){
		useragent <- "msmsMatching , msmsmatch@peakforest.fr"
		con <- PeakforestConn$new(useragent, token=token)
		res <- con$msmsSearch(spec = spec, precursor = precursor, mztol= mztol,
						  tolunit = tolunit, npmin = npmin,
						  fun = "wcosine",params = list(ppm = ppm, dmz = mzmin))
	
	
		toJSON(res)
	}
}