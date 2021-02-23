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

Rcpp::List closeMatchPpm(Rcpp::NumericVector x, Rcpp::NumericVector y,
                   Rcpp::IntegerVector xidx, Rcpp::IntegerVector yidx,
                   int xolength, double dppm, double dmz);

int ub_asc(double val, double *mzval, int first, int length);
int lb_asc(double val, double *mzval, int first, int length);

context("Testing MS matching C++ functions") {

	test_that("lb_asc() works correctly") {
		double asc_arr_1[] = {1.0, 6.0, 9.0, 16.0, 32.0, 45.0, 78.0, 100.0};
		double asc_arr_2[] = {5.0, 10.0};
    	double asc_arr_3[] = {286.1456, 287.1488, 288.1514};

		expect_true((lb_asc(101.0, asc_arr_1, 0, 8) == 7));
		expect_true((lb_asc(80.0,  asc_arr_1, 0, 8) == 6));
		expect_true((lb_asc(78.0,  asc_arr_1, 0, 8) == 6));
		expect_true((lb_asc(100.0, asc_arr_1, 0, 8) == 7));
		expect_true((lb_asc(77.0,  asc_arr_1, 0, 8) == 5));
		expect_true((lb_asc(5.0,   asc_arr_1, 0, 8) == 0));
		expect_true((lb_asc(1.0,   asc_arr_1, 0, 8) == 0));
		expect_true((lb_asc(0.0,   asc_arr_1, 0, 8) == 8));

		expect_true((lb_asc(10.0,  asc_arr_2, 0, 2) == 1));
		expect_true((lb_asc(11.0,  asc_arr_2, 0, 2) == 1));
		expect_true((lb_asc(9.0,   asc_arr_2, 0, 2) == 0));
		expect_true((lb_asc(5.0,   asc_arr_2, 0, 2) == 0));
		expect_true((lb_asc(4.0,   asc_arr_2, 0, 2) == 2));

		expect_true((lb_asc(201.0918, asc_arr_3, 0, 3) == 3));
		expect_true((lb_asc(286.145, asc_arr_3, 0, 3) == 3));
		expect_true((lb_asc(286.1456, asc_arr_3, 0, 3) == 0));
		expect_true((lb_asc(286.146, asc_arr_3, 0, 3) == 0));
		expect_true((lb_asc(287.1488, asc_arr_3, 0, 3) == 1));
		expect_true((lb_asc(287.1489, asc_arr_3, 0, 3) == 1));
		expect_true((lb_asc(288.1514, asc_arr_3, 0, 3) == 2));
		expect_true((lb_asc(289.0, asc_arr_3, 0, 3) == 2));
	}

	test_that("ub_asc() works correctly") {
		double asc_arr_1[] = {1.0, 6.0, 9.0, 16.0, 32.0, 45.0, 78.0, 100.0};
		double asc_arr_2[] = {5.0, 10.0};
    	double asc_arr_3[] = {286.1456, 287.1488, 288.1514};

		expect_true((ub_asc(101.0, asc_arr_1, 0, 8) == 8));
		expect_true((ub_asc(80.0,  asc_arr_1, 0, 8) == 7));
		expect_true((ub_asc(78.0,  asc_arr_1, 0, 8) == 6));
		expect_true((ub_asc(100.0, asc_arr_1, 0, 8) == 7));
		expect_true((ub_asc(77.0,  asc_arr_1, 0, 8) == 6));
		expect_true((ub_asc(5.0,   asc_arr_1, 0, 8) == 1));
		expect_true((ub_asc(1.0,   asc_arr_1, 0, 8) == 0));
		expect_true((ub_asc(0.0,   asc_arr_1, 0, 8) == 0));

		expect_true((ub_asc(10.0,  asc_arr_2, 0, 2) == 1));
		expect_true((ub_asc(10.1,  asc_arr_2, 0, 2) == 2));
		expect_true((ub_asc(9.0,   asc_arr_2, 0, 2) == 1));
		expect_true((ub_asc(5.1,   asc_arr_2, 0, 2) == 1));
		expect_true((ub_asc(5.0,   asc_arr_2, 0, 2) == 0));
		expect_true((ub_asc(4.9,   asc_arr_2, 0, 2) == 0));
		expect_true((ub_asc(0.0,   asc_arr_2, 0, 2) == 0));

		expect_true((ub_asc(201.0918, asc_arr_3, 0, 3) == 0));
		expect_true((ub_asc(286.145, asc_arr_3, 0, 3) == 0));
		expect_true((ub_asc(286.1456, asc_arr_3, 0, 3) == 0));
		expect_true((ub_asc(286.146, asc_arr_3, 0, 3) == 1));
		expect_true((ub_asc(287.1488, asc_arr_3, 0, 3) == 1));
		expect_true((ub_asc(287.1489, asc_arr_3, 0, 3) == 2));
		expect_true((ub_asc(288.1514, asc_arr_3, 0, 3) == 2));
		expect_true((ub_asc(289.0, asc_arr_3, 0, 3) == 3));
	}

}
