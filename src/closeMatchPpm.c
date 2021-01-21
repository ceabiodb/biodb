/* vi: se fdm=marker ts=4 et cc=80: */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "R.h"
#include "Rdefines.h"

/* Structure {{{1 */
typedef struct idxStruct {
   int from;
   int to;
} tIdxStruct;

/* Lower bound {{{1 */
/* NOT USED IN R CODE */
/*
 * Binary search inside a sorted array of the lower bound of a value.
 * 
 * val: M/Z value for which we want to find the lower bound inside the array
 * mzval: sorted (ASC or DESC?) array of M/Z values
 * first: index of the first value in the array
 * length: length of the array
 */
int lowerBound(double val, double *mzval, int first, int length) {

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

/* Upper bound {{{1 */
/* NOT USED IN R CODE */
/*
 * Binary search inside a sorted array of the upper bound of a value.
 * 
 * val: M/Z value for which we want to find the upper bound inside the array
 * mzval: sorted (ASC or DESC?) array of M/Z values
 * first: index of the first value in the array
 * length: length of the array
 */
int upperBound(double val, double *mzval, int first, int length) {
    
    int half, mid;
    
    while (length > 0) {
        half = length >> 1;
        mid = first;
        mid += half;
        if (val < mzval[mid])
            length = half;
        else {
            first = mid;
            first ++;
            length -= half + 1;
        }
    }

    return(first);
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
void fillIdxStruct(tIdxStruct *pidxS, double *px, double *py, int nx, int ny,
                   double ppm, double mzmin) {

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
        
        lb = lowerBound(py[yi] - dtol, px, lastlb, nx-lastlb);
        if (lb < nx-1)
            lastlb=lb;

        /* XXX ERROR ??? "else" of the previous "if" */
        if (lb >= nx-1) {
            lb = nx-1;
            ub = nx-1;
        }
        else /* XXX ERROR ??? should be inside "then" of "if (lb < nx-1)" */
            ub = upperBound(py[yi] + dtol, px, lb, nx-lb);

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
SEXP runMatch(tIdxStruct *pidxS, double *px, double *py, int nx, int ny,
              int *pxidx, int *pyidx, int xoLength) {
    
    int txi, from, to;
    SEXP ans, residx;

    PROTECT(ans = allocVector(VECSXP, xoLength));

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

        PROTECT(residx = NEW_INTEGER(1));

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
        INTEGER_POINTER(residx)[0] = pyidx[minindex];
        SET_VECTOR_ELT(ans, txi, residx);
        UNPROTECT(1); // residx
    }

    UNPROTECT(1); // ans
 
    return(ans);
}
 
/* Close match PPM {{{1 */
/* USED INSIDE spec-dist.R */
/*
 * x        sorted M/Z values of input spectrum (no NA)
 * y        sorted M/Z values of reference spectrum (no NA)
 * xidx     Indices of the M/Z peaks of x, taken from the original spectrum 
 *          ordered in decreasing intensity values.
 * yidx     Indices of the M/Z peaks of y, taken from the original spectrum 
 *          ordered in decreasing intensity values.
  xolength ???
  
 * dppm     ???
 * dmz      ???
 * Returns  ???
 */
SEXP closeMatchPpm(SEXP x, SEXP y, SEXP xidx, SEXP yidx,
                   SEXP xolength, SEXP dppm, SEXP dmz) {
    
    double *px = REAL(x);
    double *py = REAL(y);
    int nx = length(x);
    int ny = length(y);
    int *pxidx = INTEGER(xidx);
    int *pyidx = INTEGER(yidx);
    int xoLength = INTEGER(xolength)[0];
    double ppm = REAL(dppm)[0];
    double mzmin = REAL(dmz)[0];

    /* Allocate index structure. calloc() set memory space to zero values. */
    tIdxStruct *pidxS = (tIdxStruct*) calloc(nx, sizeof(tIdxStruct));
    if (pidxS == NULL)
        error("fastMatch/calloc: memory could not be allocated ! (%d bytes)\n",
              nx * sizeof(struct idxStruct));
    
    /* Fill index structure */
    fillIdxStruct(pidxS, px, py, nx, ny, ppm, mzmin);
    
    /* Run matching algorithm */
    SEXP ans = runMatch(pidxS, px, py, nx, ny, pxidx, pyidx, xoLength);
        
    /* Free index structure */
    free(pidxS);

    return(ans);
}
