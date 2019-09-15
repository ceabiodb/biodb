# vi: fdm=marker

# Test convertEntryIdFieldToDbClass() {{{1
################################################################

test.convertEntryIdFieldToDbClass <- function(biodb, obs) {

	# Check all databases
	for (db in biodb$getDbsInfo()$getAll())
		testthat::expect_equal(biodb$convertEntryIdFieldToDbClass(db$getEntryIdField()), db$getDbClass())

	# Check a wrong database name
	testthat::expect_null(biodb$convertEntryIdFieldToDbClass('chebi'))
	testthat::expect_null(biodb$convertEntryIdFieldToDbClass('blabla.id'))
}

# Test collapseRows() {{{1
################################################################

test.collapseRows <- function(biodb, obs) {

	# Basic tests
	testthat::expect_null(biodb$collapseRows(NULL))
	testthat::expect_identical(data.frame(), biodb$collapseRows(data.frame()))
	testthat::expect_identical(data.frame(a=1), biodb$collapseRows(data.frame(a=1)))
	testthat::expect_identical(data.frame(a=c(1,2)), biodb$collapseRows(data.frame(a=c(1,2))))
	testthat::expect_identical(data.frame(a=c(1,2)), biodb$collapseRows(data.frame(a=c(1,2,2))))
	testthat::expect_identical(data.frame(a=c(1,2),b=c('5','5|7'), stringsAsFactors=FALSE), biodb$collapseRows(data.frame(a=c(1,2,2),b=c(5,5,7))))

	# Test with a different separator
	testthat::expect_identical(data.frame(a=c(1,2),b=c('5','5;7'), stringsAsFactors=FALSE), biodb$collapseRows(data.frame(a=c(1,2,2),b=c(5,5,7)), sep=';'))

	# Test with NA values
	testthat::expect_identical(data.frame(a=c(1,NA,NA,2),b=c('5',NA,6,'5|7'), stringsAsFactors=FALSE), biodb$collapseRows(data.frame(a=c(1,NA,NA,2,2),b=c(5,NA,6,5,7))))
}

# Test entriesToDataframe() {{{1
################################################################

test.entriesToDataframe <- function(biodb, obs) {

    # Create database
    db <- data.frame(
        accession=c("A1", "A2", "A3", "A3", "A3"),
        formula=c("C3H10N2", "C6H12O6", "C6H8O7", "C6H8O7", "C6H8O7"),
        msprecmz=c(80, 90, 100, 110, 120),
        stringsAsFactors=FALSE)

    # Create connector
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(db)

    # Test
    x <- biodb$entriesToDataframe(list())
    testthat::expect_identical(data.frame(), x) 
    ids <- conn$getEntryIds()
    entries <- conn$getEntry(ids)
    x2 <- biodb$entriesToDataframe(entries, fields=character())
    testthat::expect_true(identical(data.frame(), x2))
    x3 <- biodb$entriesToDataframe(entries, fields=c('formula'))
    y3 <- unique(db['formula'])
    testthat::expect_identical(y3, x3)
    x4 <- biodb$entriesToDataframe(entries, fields=c('formula'), drop=TRUE)
    testthat::expect_identical(unique(db$formula), x4)
    x5 <- biodb$entriesToDataframe(entries, fields=c('accession', 'formula'))
    testthat::expect_identical(unique(db[c('accession', 'formula')]), x5)

    # Test with limit
    sep <- biodb$getConfig()$get('multival.field.sep')
    x7 <- biodb$entriesToDataframe(entries, limit=2, drop=TRUE, flatten=TRUE,
                                   only.atomic=FALSE, fields='msprecmz')
    y7 <- c(80, 90, paste(c(100, 110), collapse=sep))
    testthat::expect_identical(y7, x7)

    # Delete connector
    biodb$getFactory()$deleteConn(conn$getId())
}

# Test entryIdsToDataframe() {{{1
################################################################

test.entryIdsToDataframe <- function(biodb, obs) {

    # Create database
    db <- data.frame(
        accession=c("A1", "A2", "A3", "A3", "A3"),
        formula=c("C3H10N2", "C6H12O6", "C6H8O7", "C6H8O7", "C6H8O7"),
        msprecmz=c(80, 90, 100, 110, 120),
        stringsAsFactors=FALSE)

    # Create connector
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(db)

    # Test
    x <- biodb$entryIdsToDataframe(character(), db=conn$getId())
    testthat::expect_identical(data.frame(), x) 
    ids <- conn$getEntryIds()
    x2 <- biodb$entryIdsToDataframe(ids, db=conn$getId(), fields=character())
    testthat::expect_true(identical(data.frame(), x2))
    x5 <- biodb$entryIdsToDataframe(ids, db=conn$getId(),
                                    fields=c('accession', 'formula'))
    testthat::expect_identical(unique(db[c('accession', 'formula')]), x5)

    # Test with limit
    sep <- biodb$getConfig()$get('multival.field.sep')
    x7 <- biodb$entryIdsToDataframe(ids, db=conn$getId(), limit=2,
                                    fields='msprecmz')
    y7 <- data.frame(msprecmz=c(80, 90, paste(c(100, 110), collapse=sep)),
                     stringsAsFactors=FALSE)
    testthat::expect_identical(y7, x7)

    # Delete connector
    biodb$getFactory()$deleteConn(conn$getId())
}

# Test addColsToDataframe() {{{1
################################################################

test.addColsToDataframe <- function(biodb, obs) {
    testthat::expect_false(TRUE)
}

# Main {{{1
################################################################

test.that("convertEntryIdFieldToDbClass() works correctly.", 'test.convertEntryIdFieldToDbClass', biodb = biodb, obs = obs)
test.that('collapseRows() works correctly.', 'test.collapseRows', biodb = biodb, obs = obs)
test.that("entriesToDataframe() works correctly.", "test.entriesToDataframe", biodb = biodb, obs = obs)
test.that("entryIdsToDataframe() works correctly.", "test.entryIdsToDataframe", biodb = biodb, obs = obs)
test.that("addColsToDataframe() works correctly.", "test.addColsToDataframe", biodb = biodb, obs = obs)
