test_BiodbUrl <- function() {

    # Simple URL
    url <- BiodbUrl$new(url = 'https://www.somesite.fr')
    testthat::expect_equal(url$toString(), 'https://www.somesite.fr')
    url <- BiodbUrl$new(url = 'https://www.somesite.fr/')
    testthat::expect_equal(url$toString(), 'https://www.somesite.fr')
    url <- BiodbUrl$new(url = c('https://www.somesite.fr', ''))
    testthat::expect_equal(url$toString(), 'https://www.somesite.fr/')

    # URL in multiple parts
    url <- BiodbUrl$new(url = c('https://www.somesite.fr/', 'some', 'page'))
    testthat::expect_equal(url$toString(), 'https://www.somesite.fr/some/page')
    url <- BiodbUrl$new(url = c('https://www.somesite.fr//', 'some', '/page/'))
    testthat::expect_equal(url$toString(), 'https://www.somesite.fr/some/page')

    # With an unnamed parameter in a character vector
    url <- BiodbUrl$new(url = 'https://www.somesite.fr/somepage', params = c('rerun'))
    testthat::expect_equal(url$toString(), 'https://www.somesite.fr/somepage?rerun')

    # With a parameter in a character vector
    url <- BiodbUrl$new(url = 'https://www.somesite.fr/somepage', params = c(format = 'txt'))
    testthat::expect_equal(url$toString(), 'https://www.somesite.fr/somepage?format=txt')

    # With a parameter in a numeric vector
    url <- BiodbUrl$new(url = 'https://www.somesite.fr/somepage', params = c(limit = 2))
    testthat::expect_equal(url$toString(), 'https://www.somesite.fr/somepage?limit=2')

    # With two parameters in a character vector
    url <- BiodbUrl$new(url = 'https://www.somesite.fr/somepage', params = c(format = 'txt', limit = '2'))
    testthat::expect_equal(url$toString(), 'https://www.somesite.fr/somepage?format=txt&limit=2')

    # With a parameter in a list
    url <- BiodbUrl$new(url = 'https://www.somesite.fr/somepage', params = list(format = 'txt'))
    testthat::expect_equal(url$toString(), 'https://www.somesite.fr/somepage?format=txt')

    # With two parameters in a list
    url <- BiodbUrl$new(url='https://www.somesite.fr/somepage',
                    params=list(format = 'txt', limit=2))
    refUrl <-  'https://www.somesite.fr/somepage?format=txt&limit=2'
    testthat::expect_equal(url$toString(), refUrl)
}

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
    
    paramWithBracket <- BiodbUrl$new("my.site.com", c('b'='[0 TO 2]'))
    testthat::expect_equal(paramWithBracket$toString(),
        'my.site.com?b=%5B0%20TO%202%5D')
}

test_getRCurlContent <- function() {
    content <- biodb:::getRCurlContent('https://www.ebi.ac.uk/webservices/chebi/2.0/test/getCompleteEntity?chebiId=17001')
    testthat::expect_is(content, 'character')
    testthat::expect_true(length(content) == 1)
    testthat::expect_true(nchar(content) > 0)
}

# Set context
biodb::testContext("Testing BiodbUrl.")

# Run tests
biodb::testThat("BiodbUrl works fine.", test_BiodbUrl)
biodb::testThat("BiodbUrl encoding works correctly.", test_biodburl_encoding)
biodb::testThat("getRCurlContent() works fine.", test_getRCurlContent)
