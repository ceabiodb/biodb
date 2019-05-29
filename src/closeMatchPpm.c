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
void fillIdxStruct(tIdxStruct *pidxS, double *px, double *py, int nx, int ny,
                   double ppm, double mzmin) {

    double dtol;
    int lb, ub;
    int lastlb = 0;
    
    /* Initialize from slot */
    tIdxStruct *end = pidxS + nx;
    for (tIdxStruct *p = pidxS ; p < end ; ++p)
        p->from = ny + 1;

    for (int yi=0 ; yi < ny ; ++yi) {

        dtol = py[yi] * ppm * 1e-6;
        if(dtol < mzmin)
            dtol = mzmin;

        lb = lowerBound(py[yi] - dtol, px, lastlb, nx-lastlb);
        if (lb < nx-1)
            lastlb=lb;

        if (lb >= nx-1) {
            lb = nx-1;
            ub = nx-1;
        }
        else
            ub = upperBound(py[yi] + dtol, px, lb, nx-lb);

        if (ub > nx-1)
            ub = nx -1;

        for (int xi=lb;xi <= ub;xi++) {
            if (fabs(py[yi] - px[xi]) <= dtol) {
                if (yi < pidxS[xi].from)
                    pidxS[xi].from = yi;
                if (yi > pidxS[xi].to)
                    pidxS[xi].to = yi;
            }
       }
    }
}

/* Run match {{{1 */
SEXP runMatch(tIdxStruct* pidxS, double *px, double *py, int nx, int ny,
              int *pxidx, int *pyidx, int xoLength) {
    
    int txi, from, to;
    SEXP ans, residx;

    PROTECT(ans = allocVector(VECSXP, xoLength));

    for (int xi=0;xi < nx;xi++) {

        // no match
        if (pidxS[xi].from == ny +1 && pidxS[xi].to == 0)
            continue;

        txi = pxidx[xi] -1;
        from = pidxS[xi].from;
        to = pidxS[xi].to;

        // single match
        if (pidxS[xi].from == ny + 1)
            from = pidxS[xi].to;
        if (pidxS[xi].to == 0)
            to = pidxS[xi].from;

        PROTECT(residx = NEW_INTEGER(1));

        //Checking which point is the closest.
        double mindist = 10;
        int minindex   = -1;
        for (int yi=from;yi <= to;yi++)
            if(fabs(py[yi] - px[xi])<mindist)
                minindex=yi;
        INTEGER_POINTER(residx)[0] = pyidx[minindex];
        SET_VECTOR_ELT(ans, txi, residx);
        UNPROTECT(1); // residx
    }

    UNPROTECT(1); // ans
 
    return(ans);
}
 
/* Close match PPM {{{1 */
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

    /* Allocate index structure */
    tIdxStruct *pidxS = (tIdxStruct*) calloc(nx, sizeof(tIdxStruct));
    if (pidxS == NULL)
        error("fastMatch/calloc: memory could not be allocated ! (%d bytes)\n",
              nx * sizeof(struct idxStruct));
    
    /* Fill index structure */
    fillIdxStruct(pidxS, px, py, nx, ny, ppm, mzmin);
    
    /* match */
    SEXP ans = runMatch(pidxS, px, py, nx, ny, pxidx, pyidx, xoLength);
        
    /* Free index structure */
    free(pidxS);

    return(ans);
}
