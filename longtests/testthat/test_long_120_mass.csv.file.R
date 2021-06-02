MASSFILEDB.URL <- system.file("extdata", "massbank_extract_full.tsv",
                              package="biodb")

test.mass.csv.file.output.columns <- function(db) {

    biodb <- db$getBiodb()

    # Open database file
    db.df <- read.table(db$getPropValSlot('urls', 'base.url'), sep="\t",
                        header=TRUE, quote='"', stringsAsFactors=FALSE,
                        row.names=NULL)

    # Get all entries
    entries <- db$getEntry(db$getEntryIds())
    testthat::expect_gte(length(entries), 1)

    # Get data frame
    entries.df <- biodb$entriesToDataframe(entries, only.atomic=FALSE)
    testthat::expect_gte(nrow(entries.df), 1)
    testthat::expect_gte(ncol(entries.df), 5)

    # Check that all columns of database file are found in entries data frame
    # NOTE This supposes that the columns of the database file are named
    # according to biodb conventions.
    msg <- paste("Columns ",
        paste(colnames(db.df)[! colnames(db.df) %in% colnames(entries.df)],
              collapse=', '), " are not included in output.", sep='')
    testthat::expect_true(all(colnames(db.df) %in% colnames(entries.df)), msg)
}

test.mass.csv.file.precursor.match <- function(biodb) {

    # Set retention time values
    prec.112.rt <- 5.69
    prec.108.rt <- 8.75
    precursor.rt.tol <- 1
    rt.tol <- 5
    rt.tol.exp <- 0.8

    # Define db data frame
    db.df <- rbind(
                   data.frame(accession='C1', ms.mode='pos', peak.mztheo=112, peak.comp='P9Z6W410 O', peak.attr='[(M+H)-(H2O)-(NH3)]+', formula="J114L6M62O2", molecular.mass=146.10553, name='Blablaine', chrom.col.id="col1", chrom.rt=prec.112.rt, chrom.rt.unit='min'),
                   data.frame(accession='C1', ms.mode='pos', peak.mztheo=54,  peak.comp='P9Z6W410 O', peak.attr='[(M+H)-(NH3)]+', formula="J114L6M62O2", molecular.mass=146.10553, name='Blablaine', chrom.col.id="col1", chrom.rt=prec.112.rt, chrom.rt.unit='min'),
                   data.frame(accession='A2', ms.mode='pos', peak.mztheo=112, peak.comp='P9Z6W410 O', peak.attr='[(M+H)]+', formula="J114L6M62O2", molecular.mass=146.10553, name='Blablaine', chrom.col.id="col1", chrom.rt=prec.112.rt, chrom.rt.unit='min'),
                   data.frame(accession='A2', ms.mode='pos', peak.mztheo=69,  peak.comp='P9Z6W410 O', peak.attr='[(M+H)-(NH3)]+', formula="J114L6M62O2", molecular.mass=146.10553, name='Blablaine', chrom.col.id="col1", chrom.rt=prec.112.rt, chrom.rt.unit='min'),
                   data.frame(accession='A2', ms.mode='pos', peak.mztheo=54,  peak.comp='P9Z6W410 O', peak.attr='[(M+H)-(H2O)]+', formula="J114L6M62O2", molecular.mass=146.10553, name='Blablaine', chrom.col.id="col1", chrom.rt=prec.112.rt, chrom.rt.unit='min'),
                   data.frame(accession='B3', ms.mode='pos', peak.mztheo=108, peak.comp='P9Z6W410 O', peak.attr='[(M+H)]+', formula="J114L6M62O2", molecular.mass=146.10553, name='Blablaine', chrom.col.id="col1", chrom.rt=prec.108.rt, chrom.rt.unit='min'),
                   stringsAsFactors=FALSE)

    # Create connector
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(db.df)

    # Input
    mz <- c(54,          112,         108)
    rt <- c(prec.112.rt, prec.112.rt, prec.108.rt + precursor.rt.tol + 1e-6)

    # M/Z Search
    results <- conn$searchMsPeaks(mz=mz, mz.tol=0.1, precursor=TRUE)
    expect_is(results, 'data.frame')
    expect_equal(nrow(results), 3)
    expect_true(all(results[['accession']] %in% c('A2', 'B3')))

    # With precursor RT tolerance
    results2 <- conn$searchMsPeaks(mz=mz, rt=rt, mz.tol=0.1, precursor=TRUE,
                                   precursor.rt.tol=precursor.rt.tol,
                                   chrom.col.ids='col1', rt.tol=rt.tol ,
                                   rt.tol.exp=rt.tol.exp, rt.unit='min')
    expect_is(results2, 'data.frame')
    expect_equal(nrow(results2), 3)
    expect_identical(results2[['accession']], c('A2', 'A2', NA_character_))
}

test.mass.csv.file.rt.matching.limits <- function(biodb) {

    # Define db data frame
    db.df <- rbind(data.frame(), list(accession = 'C1', ms.mode = 'POS', peak.mztheo = 112.07569, peak.comp = 'P9Z6W410 O', peak.attr = '[(M+H)-(H2O)-(NH3)]+', chrom.col.id = "col1", chrom.rt = 5.69, chrom.rt.unit = 'min', formula = "J114L6M62O2", molecular.mass = 146.10553, name = 'Blablaine'), stringsAsFactors = FALSE)

    # Create connector
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(db.df)

    # M/Z matching values
    mz.tol <- 5
    mz.tol.unit <- 'ppm'
    mz <- db.df[['peak.mztheo']]

    # RT matching values
    x <- 5.0
    y <- 0.8
    col.id <-db.df[['chrom.col.id']]
    rt <- db.df[['chrom.rt']]
    rt.sec <- if (db.df[['chrom.rt.unit']] == 'min') rt * 60 else rt
    rt.unit <- 's'
    rt.sup <- uniroot(function(rt) rt - x - (rt) ^ y - rt.sec, c(0,1000))$root
    rt.inf <- uniroot(function(rt) rt + x + (rt) ^ y - rt.sec, c(0,1000))$root

    # Search
    results1 <- conn$searchMsPeaks(mz, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit,
                                   ms.mode='pos', chrom.col.ids=col.id,
                                   rt=rt.sec, rt.unit=rt.unit, rt.tol=x,
                                   rt.tol.exp=y)
    expect_is(results1, 'data.frame')
    results2 <- conn$searchMsPeaks(mz, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit,
                                   ms.mode='pos', chrom.col.ids=col.id,
                                   rt=(rt.inf + rt.sup) / 2, rt.unit=rt.unit,
                                   rt.tol=x, rt.tol.exp=y)
    expect_is(results2, 'data.frame')
    results3 <- conn$searchMsPeaks(mz, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit,
                                   ms.mode='pos', chrom.col.ids=col.id,
                                   rt=rt.inf, rt.unit=rt.unit, rt.tol=x,
                                   rt.tol.exp=y)
    expect_is(results3, 'data.frame')
    results4 <- conn$searchMsPeaks(mz, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit,
                                   ms.mode='pos', chrom.col.ids=col.id,
                                   rt=rt.inf - 1e-6, rt.unit=rt.unit, rt.tol=x,
                                   rt.tol.exp=y, insert.input.values=FALSE)
    expect_identical(results4, data.frame())
    results4.1 <- conn$searchMsPeaks(mz, mz.tol=mz.tol,
                                     mz.tol.unit=mz.tol.unit, ms.mode='pos',
                                     chrom.col.ids=col.id, rt=rt.inf - 1e-6,
                                     rt.unit=rt.unit, rt.tol=x, rt.tol.exp=y,
                                     insert.input.values=TRUE)
    expect_is(results4.1, 'data.frame')
    expect_identical(colnames(results4.1), c('mz', 'rt'))
    results5 <- conn$searchMsPeaks(mz, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit,
                                   ms.mode='pos', chrom.col.ids=col.id,
                                   rt=rt.sup, rt.unit=rt.unit, rt.tol=x,
                                   rt.tol.exp=y)
    expect_is(results5, 'data.frame')
    results6 <- conn$searchMsPeaks(mz, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit,
                                   ms.mode='pos', chrom.col.ids=col.id,
                                   rt=rt.sup + 1e-6, rt.unit=rt.unit, rt.tol=x,
                                   rt.tol.exp=y, insert.input.values=FALSE)
    expect_identical(results6, data.frame())
    results6.1 <- conn$searchMsPeaks(mz, mz.tol=mz.tol,
                                     mz.tol.unit=mz.tol.unit, ms.mode='pos',
                                     chrom.col.ids=col.id, rt=rt.sup + 1e-6,
                                     rt.unit=rt.unit, rt.tol=x, rt.tol.exp=y,
                                     insert.input.values=TRUE)
    expect_is(results6.1, 'data.frame')
    expect_identical(colnames(results6.1), c('mz', 'rt'))
}

# Set context
biodb::testContext("MassCsvFile long generic tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Create connector
conn <- biodb$getFactory()$createConn('mass.csv.file', url=MASSFILEDB.URL)

# Make sure we have no residual cache entries from previous tests
biodb$getPersistentCache()$deleteAllFiles(conn$getCacheId(), fail=FALSE)

# Run generic tests
biodb::runGenericTests(conn, short=FALSE, long=TRUE)

# Set context
biodb::testContext("MassCsvFile long specific tests")

biodb::testThat("M/Z match output contains all columns of database.",
                test.mass.csv.file.output.columns, conn=conn)
biodb::testThat('Precursor match works.', test.mass.csv.file.precursor.match,
                biodb=biodb)
biodb::testThat('RT matching limits (rt.min and rt.max) are respected.',
                test.mass.csv.file.rt.matching.limits, biodb=biodb)

# Terminate Biodb
biodb$terminate()
