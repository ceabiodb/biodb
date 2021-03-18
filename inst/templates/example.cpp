/* vi: se fdm=marker ts=4 et cc=80: */

#include <Rcpp.h>

// Multiply a vector by a scalar value.
//
// Use pointers to speed up computing.
//
// This C++ function cannot be reached from R code.
// It is thus tested inside C++ code inside the test-100-example.cpp file.
void vectScalar(const double* v, double*w, size_t n, double a) {
    
    const double* end = v + n;
    for ( ; v != end ; ++v, ++w)
        *w += a * *v;
}

//' Convolution.
//'
//' Example function taken from Rcpp vignette "Rcpp-introduction".
//' This function is callable from R as a normal R function, it is thus tested 
//' as a normal R function.
//'
//' @param a     A numeric vector.
//' @param b     A numeric vector.
//' @return  The result as a numeric vector.
//'
// [[Rcpp::export]]
Rcpp::NumericVector convolve_cpp(const Rcpp::NumericVector& a,
                                 const Rcpp::NumericVector& b) {

    // Declare loop counters, and vector sizes
    size_t na = a.size(), nb = b.size(), nab = na + nb -1;
    
    // Create vector filled with 0
    Rcpp::NumericVector ab(nab);
    
    // Crux of the algorithm
    const double *a_end = &a[0] + na;
    const double *b_start = &b[0];
    double *q = &ab[0];
    for(const double *p = &a[0] ; p != a_end ; ++p, ++q)
        vectScalar(b_start, q, nb, *p);

    // Return result
    return ab;
}
