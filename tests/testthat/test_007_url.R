test_biodburl_encoding <- function() {

    testthat::expect_equal(BiodbUrl$new("my.site.com")$toString(), 'my.site.com')
    testthat::expect_equal(BiodbUrl$new("my.site.com", c('a'))$toString(),
                           'my.site.com?a')
    testthat::expect_equal(BiodbUrl$new("my.site.com", c('a'=2))$toString(),
                           'my.site.com?a=2')
    testthat::expect_equal(BiodbUrl$new("my.site.com", c('a'=2, 'n'))$toString(),
                           'my.site.com?a=2&n')
    urlWithSpace <- BiodbUrl$new("my.site.com", c('a b'=2, 'n'))
    testthat::expect_equal(urlWithSpace$toString(), 'my.site.com?a%20b=2&n')
    testthat::expect_equal(urlWithSpace$toString(FALSE), 'my.site.com?a b=2&n')
    urlWithSpace <- BiodbUrl$new("my.site.com", c('a b'='my value', 'n'))
    testthat::expect_equal(urlWithSpace$toString(),
                           'my.site.com?a%20b=my%20value&n')
    testthat::expect_equal(urlWithSpace$toString(FALSE),
                           'my.site.com?a b=my value&n')
    testthat::expect_equal(BiodbUrl$new('my site')$toString(), 'my%20site')
    testthat::expect_equal(BiodbUrl$new('my site')$toString(FALSE), 'my site')
}

# Set context
biodb::testContext("Testing BiodbUrl.")

# Run tests
biodb::testThat("BiodbUrl encoding works correctly.", test_biodburl_encoding)
