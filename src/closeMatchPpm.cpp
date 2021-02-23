/* vi: se fdm=marker ts=4 et cc=80: */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <Rcpp.h>

/* Structure {{{1 */
typedef struct idxStruct {
   int from;
   int to;
   idxStruct() {
       this->from = this->to = 0;
   }
} tIdxStruct;

/* Upper bound {{{1 */
/* NOT USED IN R CODE */
/*
 * Find the upper bound of a value inside an array sorted in ascending order.
 * 
 * val:     M/Z value for which we want to find the lower bound inside the array.
 * mzval:   Sorted array of M/Z values.
 * first:   Index of the first value in the array.
 * length:  Number of values in which to search.
 * Returns: The index of the upper bound, or the value of length if no bound
 *          was found.
 */
// TODO XXX Replace by STL lower_bound once in Rcpp
// See https://www.studytonight.com/cpp/stl/stl-searching-lower-upper-bound
int ub_asc(double val, double *mzval, int first, int length) {

    int half, mid;

    while (length > 0) {
        half = length >> 1;
        mid = first;
        mid += half;
        if (mzval[mid] < val) {
            first = mid;
            ++first;
            length -= half + 1;
        }
        else
            length = half;
    }

    return(first);
}

/* Lower bound {{{1 */
/* NOT USED IN R CODE */
/*
 * Find the lower bound of a value inside an array sorted in ascending order.
 * 
 * val:     M/Z value for which we want to find the lower bound inside the array.
 * mzval:   Sorted array of M/Z values.
 * first:   Index of the first value in the array.
 * length:  Number of values in which to search.
 * Returns: The index of the lower bound, or .
 */
int lb_asc(double val, double *mzval, int first, int length) {

    int half, mid;
    
    if (val < mzval[first])
	    first += length;
	else
    	while (length > 0) {
	    	if (length == 1) {
		    	if (val >= mzval[first])
			    	break;
		    	first += length;
		    	break;
			}
	    	if (length == 2) {
		    	if (val >= mzval[first + 1]) {
			    	++first;
			    	break;
				}
		    	if (val >= mzval[first])
			    	break;
		    	first += length;
		    	break;
			}
		    	
        	half = length >> 1;
        	mid = first + half;
        	if (val < mzval[mid])
            	length = half;
        	else {
            	first = mid;
            	length -= half;
        	}
    	}

    return first;
}

/* Fill idx struct {{{1 */
/* NOT USED IN R CODE */
/*
 * Search for each M/Z value of px, which M/Z values of py are close, given a
 * tolerance. For each M/Z value of px, there is a structure inside pidxS that
 * records the range ([from, to]) of indices inside py.
 * 
 * pidxS: an array of tIdxStruct structures with two fields "from" and "to".
 * px: a sorted array of M/Z values.
 * py: a sorted array of M/Z values.
 * nx: length of px and pidxS.
 * ny: length of py.
 * ppm: M/Z tolerance in PPM
 * mzmin: minimum M/Z tolerance
 */
void fillIdxStruct(tIdxStruct *pidxS, double *px, double *py,
                   int nx, int ny, double ppm, double mzmin) {

    double dtol;
    int lb, ub;
    int lastlb = 0;
    
    /* Initialize from slot */
    tIdxStruct *end = pidxS + nx;
    for (tIdxStruct *p = pidxS ; p < end ; ++p)
        p->from = ny + 1;
    /* p->to already set to 0 by calloc() */

    /* Loop on all M/Z values of py */
    for (int yi=0 ; yi < ny ; ++yi) {

        /* Compute tolerance in PPM */
        dtol = py[yi] * ppm * 1e-6;
        if(dtol < mzmin)
            dtol = mzmin;

        /* We look for all M/Z values of px that are inside py[yi] +- dtol */
        
        lb = ub_asc(py[yi] - dtol, px, lastlb, nx-lastlb);
        if (lb < nx-1)
            lastlb=lb;

        /* XXX ERROR ??? "else" of the previous "if" */
        if (lb >= nx-1) {
            lb = nx-1;
            ub = nx-1;
        }
        else /* XXX ERROR ??? should be inside "then" of "if (lb < nx-1)" */
            ub = lb_asc(py[yi] + dtol, px, lb, nx-lb);

        if (ub > nx-1)
            ub = nx -1;

        /* We loop on all M/Z values of px that are inside py[yi] +- dtol */
        for (int xi=lb;xi <= ub;xi++) {
            
            /* XXX ERROR? Is this condition not necessarily true? */
            if (fabs(py[yi] - px[xi]) <= dtol) {
                
                // We update the "from", i.e.: the first index of the M/Z
                // values in py for which we have a match with this M/Z value
                // of px
                if (yi < pidxS[xi].from)
                    pidxS[xi].from = yi;
                
                // We update the "to", i.e.: the last index of the M/Z
                // values in py for which we have a match with this M/Z value
                // of px
                if (yi > pidxS[xi].to)
                    pidxS[xi].to = yi;
            }
       }
    }
}

/* Run match {{{1 */
/* NOT USED IN R CODE */
/*
 * 
 * 
 * pidxS:
 * px:
 * py:
 * nx:
 * ny:
 * pxidx:
 * pyidx:
 * xoLength:
 */
Rcpp::List runMatch(tIdxStruct *pidxS, double * px, double *py, int nx, int ny,
              int *pxidx, int *pyidx, int xoLength) {
    
    int txi, from, to;
    Rcpp::List ans(xoLength);

    tIdxStruct *end = pidxS + nx;
    double *q = px;
    int *i = pxidx;
    
    for (tIdxStruct *p = pidxS ; p < end ; ++p, ++q, ++i) {
        
        // no match
        if (p->from == ny +1 && p->to == 0)
            continue;

        txi = *i - 1;
        from = p->from == (ny + 1) ? p->to : p->from;
        to = p->to == 0 ? p->from : p->to;

        //Checking which point is the closest.
        double mindist = 10;
        int minindex   = -1;
        int yi = from;
        double *rend = py + to;
        for (double *r = py + from ; r <= rend ; ++r, ++yi)
            if(fabs(*r - *q) < mindist) {
                minindex = yi;
                /* XXX ERROR Shouldn't we set "mindist = fabs(*r - *q)"? */
            }
        /* XXX ERROR What if minindex == -1? */
        ans[txi] = pyidx[minindex];
    }

    // If there was no match at all, `ans` is NULL.
//    UNPROTECT(1); // ans
 
    return(ans);
}
 
// Close match ppm {{{1
//////////////////////////////////////////////////////////////// 

//' Close match PPM
//'
//' Matches peaks between two spectra.
//'
//' @param x        sorted M/Z values (ascending order) of input spectrum (no NA).
//' @param y        sorted M/Z values (ascending order) of reference spectrum (no NA).
//' @param xidx     indices of the M/Z peaks of x, taken from the original spectrum 
//'          ordered in decreasing intensity values.
//' @param yidx     indices of the M/Z peaks of y, taken from the original spectrum 
//'          ordered in decreasing intensity values.
//' @param xolength ???
//' @param dppm     ???
//' @param dmz      ???
//' @return  ???
//'
// [[Rcpp::export]]
Rcpp::List closeMatchPpm(Rcpp::NumericVector x, Rcpp::NumericVector y,
                         Rcpp::IntegerVector xidx, Rcpp::IntegerVector yidx,
                         int xolength, double dppm, double dmz) {
    
    /* Allocate index structure. calloc() set memory space to zero values. */
    tIdxStruct *pidxS = new tIdxStruct[x.length()];
    if ( ! pidxS)
        Rcpp::stop("Could not be allocate %d bytes of memory.\n",
              x.length() * sizeof(tIdxStruct));
    
    /* Fill index structure */
    fillIdxStruct(pidxS, &x[0], &y[0], x.length(), y.length(), dppm, dmz);
    
    /* Run matching algorithm */
    Rcpp::List ans = runMatch(pidxS, &x[0], &y[0], x.length(), y.length(),
                              &xidx[0], &yidx[0], xolength);
        
    /* Free index structure */
    free(pidxS);

    return(ans);
}
