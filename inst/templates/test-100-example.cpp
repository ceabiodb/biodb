/*
 * This file uses the Catch unit testing library, alongside
 * testthat's simple bindings, to test a C++ function.
 *
 * For your own packages, ensure that your test files are
 * placed within the `src/` folder, and that you include
 * `LinkingTo: testthat` within your DESCRIPTION file.
 */

#include <testthat.h>
#include <Rcpp.h>

// We need to declare the function we want to test.
void vectScalar(const double* v, double*w, size_t n, double a);

context("Example of testing C++ functions") {

	test_that("vectScalar() works correctly") {
		double a[] = {1.0, 1.0, 1.0, 1.0};
		double b[] = {5.0, 6.0};
		double r[] = {1.0, 11.0, 13.0, 1.0};
		vectScalar(b, a + 1, 2, 2.0);
		for (double* p = a, *q = r ; p < a + 4 ; ++p, ++q)
			expect_true(*p == *q);
	}

}
