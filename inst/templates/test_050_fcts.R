# $$$ SECTION RCPP $$$
test_convolve_cpp <- function() {
    testthat::expect_equal(convolve_cpp(1:5, 3:7),
                           c(3, 10, 22, 40, 65, 72, 70, 58, 35))
}
# $$$ END_SECTION RCPP $$$

# Set test context
biodb::testContext("Tests of independent functions")

# Run tests
# $$$ SECTION RCPP $$$
biodb::testThat("Convolve function works correctly", test_convolve_cpp)
# $$$ END_SECTION RCPP $$$
# TODO Implement your own tests
