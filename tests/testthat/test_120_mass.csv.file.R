MASSFILEDB.URL <- system.file("extdata", "massbank_extract_full.tsv",
                              package="biodb")
MASSFILEDB.WRONG.HEADER.URL <- file.path(getwd(), 'res',
                                         'mass.csv.file-wrong_header.tsv')
MASSFILEDB.WRONG.NB.COLS.URL <- file.path(getwd(), 'res',
                                          'mass.csv.file-wrong_nb_cols.tsv')

test.basic.mass.csv.file <- function(db) {

    # Open file
    df <- read.table(db$getPropValSlot('urls', 'base.url'), sep="\t",
                     header=TRUE, quote='"', stringsAsFactors=FALSE,
                     row.names=NULL)

    # Test number of entries
    expect_gt(db$getNbEntries(), 1)
    expect_equal(db$getNbEntries(), sum( ! duplicated(df[c('accession')])))

    # Get an entry ID
    id <- df[df[['ms.level']] == 1 & ! is.na(df[['chrom.col.id']]),
             'accession'][[1]]

    # Test number of peaks
    expect_gt(db$getNbPeaks(), 1)
    expect_gt(db$getNbPeaks(mode='neg'), 1)
    expect_gt(db$getNbPeaks(mode='pos'), 1)
    expect_equal(db$getNbPeaks(), nrow(df))
    expect_gt(db$getNbPeaks(ids=id), 1)

    # Test chrom cols
    expect_gt(nrow(db$getChromCol()), 1)
    expect_gte(nrow(db$getChromCol(ids=id)), 1)
    expect_lte(nrow(db$getChromCol(ids=id)), nrow(db$getChromCol()))
    expect_true(all(db$getChromCol(ids=id)[['id']] %in%
                    db$getChromCol()[['id']]))

    # Test mz values
    expect_true(is.vector(db$getMzValues()))
    expect_gt(length(db$getMzValues()), 1)
    expect_error(db$getMzValues('wrong.mode.value'), silent=TRUE)
    expect_gt(length(db$getMzValues('neg')), 1)
    expect_gt(length(db$getMzValues('pos')), 1)
}

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

test.mass.csv.file.data.frame <- function(biodb) {

    # Define database data frame
    ids <- 'ZAP'
    mz <- 12
    df <- data.frame(accession=ids, mz=mz, mode='+')

    # New biodb instance
    conn <- biodb$getFactory()$createConn('mass.csv.file')

    # Set database
    conn$setDb(df)

    # Set fields
    conn$setField('accession', 'accession')
    conn$setField('ms.mode', 'mode')
    conn$setField('peak.mztheo', 'mz')

    # Get M/Z values
    expect_identical(mz, conn$getMzValues())
    expect_identical(ids, conn$getEntryIds())
}

test.mass.csv.file.old.col.names <- function(biodb) {

    # Define data frame
    df <- data.frame(compoundid='A10', msmode='POS', mztheo=112.07569,
                     peakcomp='P9Z6W410', peakattr='[(M+H)-(H2O)-(NH3)]+',
                     chromcol='colAA', chromcolrt=94.8,
                     compoundcomp='J114L6M62O2', compoundmass=146.10553,
                     fullnames='Blablaine')

    # New connector instance
    conn <- biodb$getFactory()$createConn('mass.csv.file')

    # Set database
    conn$setDb(df)

    # Get chrom cols
    cols <- conn$getChromCol()
    expect_is(cols, "data.frame")
}

test.fields <- function(biodb) {

    # Get fields
    col.name <- conn$getFieldColName('ms.mode')
    expect_is(col.name, 'character')
    expect_true(nchar(col.name) > 0)

    # Test if has field
    testthat::expect_true(conn$hasField('accession'))
    testthat::expect_false(conn$hasField('blabla'))
    testthat::expect_false(conn$hasField('nt.seq'))

    # Add new field
    conn$addField('nt.seq', 'N')

    # Set wrong fields
    testthat::expect_error(conn$setField('invalid.tag.name', colname='something'),
                 regexp='^.*Database field "invalid.tag.name" is not valid.$')
    testthat::expect_error(conn$setField('ms.mode', colname='wrong.col.name'),
                 regexp='^.*Column.* is/are not defined in database file.$')

    # Reseting accession field should fail
    testthat::expect_error(conn$setField('accession', 'compound.id'))
}

test.undefined.fields <- function(biodb) {

    conn <- biodb$getFactory()$createConn('mass.csv.file',
                                          url=MASSFILEDB.WRONG.HEADER.URL)
    
    testthat::expect_warning(conn$hasField('accession'),
        regexp='^.*Column ".*" does not match any biodb field\\.$')

    testthat::expect_error(conn$getEntryIds(),
        regexp='^.*Field.* accession is/are undefined in file database\\.$')

    biodb$getFactory()$deleteConn(conn$getId())
}

test.wrong.nb.cols <- function(biodb) {
    conn <- biodb$getFactory()$createConn('mass.csv.file',
                                          url=MASSFILEDB.WRONG.NB.COLS.URL)
    expect_error(ids <- conn$getEntryIds(),
                 regexp='^line 1 did not have 12 elements$')
    biodb$getFactory()$deleteConn(conn$getId())
}

test.field.card.one <- function(biodb) {

    # Define database data frame
    ids <- c('ZAP', 'ZAP')
    mzs <- c(12, 14)
    modes <- c('+', '-')
    df <- data.frame(ids=ids, mz=mzs, mode=modes)

    # Create connector
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(df)

    # Set fields
    conn$setField('accession', 'ids')
    conn$setField('ms.mode', 'mode')
    conn$setField('peak.mztheo', 'mz')
    expect_error(conn$checkDb(),
                 regexp=paste0('^.*Cannot set more that one value .* into',
                               ' single value field .*\\.$'))
}

test.getMzValues.without.peak.attr <- function(biodb) {

    df <- rbind(data.frame(),
                list(accession='BML80005', ms.mode='pos', ms.level=1, peak.mztheo=219.1127765, peak.intensity=373076,  peak.relative.intensity=999),
                stringsAsFactors=FALSE)

    # Create connector
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(df)

    # Get M/Z values
    mzs <- conn$getMzValues(max.results=10)
    expect_is(mzs, 'numeric')
    expect_length(mzs, 1)

    # Get M/Z values filtering on precursor
    mzs <- conn$getMzValues(max.results=10, precursor=TRUE, ms.mode='pos')
    expect_is(mzs, 'numeric')
    expect_length(mzs, 0)
}

test.mass.csv.file.entry.peaks.table.col.names <- function(biodb) {

    # Define db data frame
    id <- 'BML80005'
    df <- rbind(data.frame(), list(accession=id, mode='pos', mztheo=219.1127765,
                                   peakcomp='CHON',
                                   peakattr='[(M+H)-(H2O)-(NH3)]+', int=373076,
                                   relint=999), stringsAsFactors=FALSE)

    # Create connector
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(df)
    conn$setField('ms.mode', 'mode')
    conn$setField('peak.mztheo', 'mztheo')
    conn$setField('peak.attr', 'peakattr')
    conn$setField('peak.comp', 'peakcomp')
    conn$setField('peak.intensity', 'int')
    conn$setField('peak.relative.intensity', 'relint')

    # Get entry and peaks
    entry <- biodb$getFactory()$getEntry(conn$getId(), id)
    expect_is(entry, 'MassCsvFileEntry')
    peaks <- entry$getFieldValue('peaks')
    expect_is(peaks, 'data.frame')
    expect_equal(nrow(peaks), 1)
    expect_gte(ncol(peaks), 1)

    # Check that colnames of peaks table are all valid field names or aliases
    expect_true(all(vapply(names(peaks), function(field) biodb$getEntryFields()$isDefined(field), FUN.VALUE = FALSE)))

    # Check that colnames of peaks table are all official field names (not aliases)
    expect_true(all(vapply(names(peaks), function(field) biodb$getEntryFields()$get(field)$getName() == field, FUN.VALUE = FALSE)))
}

test.mass.csv.file.searchMsPeaks.column.sorting <- function(biodb) {

    # Define db data frame
    db.df <- rbind(data.frame(accession='C1', ms.mode='POS', peak.mztheo=112.07569, peak.comp='P9Z6W410 O', peak.attr='[(M+H)-(H2O)-(NH3)]+', formula="J114L6M62O2", molecular.mass=146.10553, name='Blablaine', inchi='InChI=1S/C7H8N4O3/c1-10-4-3(8-6(10)13)5(12)11(2)7(14)9-4/h1-2H3,(H,8,13)(H,9,14)', inchikey='NA'),
                   data.frame(accession='C2', ms.mode='POS', peak.mztheo=208.8274, peak.comp='Y2Z6W410 O', peak.attr='[(M+H)-(H2O)]+', formula="T114L6M62O2", molecular.mass=146.10553, name='Sloopaine', inchi='InChI=1S/C7H8N4O3/c1-10-4-3(8-6(10)13)5(12)11(2)7(14)9-4/h1-2H3,(H,8,13)(H,9,14)', inchikey='FRYVQXCDSBCDAU-AKDOOQIZSA-N'),
                   stringsAsFactors=FALSE)

    # Create connector
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(db.df)

    # Search
    results <- conn$searchMsPeaks(db.df[['peak.mztheo']], mz.tol = 10, insert.input.values = FALSE)
    expect_is(results, 'data.frame')
    expected.cols <- sort(c(colnames(db.df), 'peak.mz', 'mass.csv.file.id'))
    expect_identical(expected.cols, colnames(results))
}

test.mass.csv.file.mz.matching.limits <- function(biodb) {

    # Define db data frame
    db.df <- rbind(data.frame(), list(accession='C1', ms.mode='POS',
                                      peak.mztheo=112.07569,
                                      peak.comp='P9Z6W410 O',
                                      peak.attr='[(M+H)-(H2O)-(NH3)]+',
                                      formula="J114L6M62O2",
                                      molecular.mass=146.10553,
                                      name='Blablaine'),
                   stringsAsFactors=FALSE)

    # Create connector
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(db.df)

    mz.tol <- 5
    # Compute mz.min and mz.max
    mz.tol.unit <- 'ppm'
    mz <- db.df[['peak.mztheo']]
    mz.sup <- mz / ( 1 + ( - mz.tol) * 1e-6)
    mz.sup <- mz.sup - 1e-8 # Adjustment needed, due to computing differences.
    mz.inf <- mz / ( 1 +  ( + mz.tol) * 1e-6)

    # Search
    results1 <- conn$searchMsPeaks(mz, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit, ms.mode='pos')
    expect_is(results1, 'data.frame')
    results2 <- conn$searchMsPeaks((mz.inf + mz.sup)/2, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit, ms.mode='pos')
    expect_is(results2, 'data.frame')
    results3 <- conn$searchMsPeaks(mz.inf, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit, ms.mode='pos')
    expect_is(results3, 'data.frame')
    results4 <- conn$searchMsPeaks(mz.inf - 1e-6, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit, ms.mode='pos', insert.input.values=FALSE)
    expect_identical(results4, data.frame())
    results4.1 <- conn$searchMsPeaks(mz.inf - 1e-6, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit, ms.mode='pos', insert.input.values=TRUE)
    expect_is(results4.1, 'data.frame')
    expect_identical(colnames(results4.1), 'mz')
    results5 <- conn$searchMsPeaks(mz.sup, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit, ms.mode='pos')
    expect_is(results5, 'data.frame')
    results6 <- conn$searchMsPeaks(mz.sup + 1e-6, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit, ms.mode='pos', insert.input.values=FALSE)
    expect_identical(results6, data.frame())
    results6.1 <- conn$searchMsPeaks(mz.sup + 1e-6, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit, ms.mode='pos', insert.input.values=TRUE)
    expect_is(results6.1, 'data.frame')
    expect_identical(colnames(results6.1), 'mz')
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

test.mass.csv.file.ms.mode.values <- function(biodb) {

    # Define db data frame
    db.df <- rbind(data.frame(), list(accession='C1', ms.mode='ZZZ',
                                      peak.mztheo=112.07569,
                                      peak.comp='P9Z6W410 O',
                                      peak.attr='[(M+H)-(H2O)-(NH3)]+',
                                      formula="J114L6M62O2",
                                      molecular.mass=146.10553,
                                      name='Blablaine'),
                   stringsAsFactors=FALSE)

    # Add new MS mode value
    biodb$getEntryFields()$get('ms.mode')$addAllowedValue('pos', 'POS')
    expect_error(biodb$getEntryFields()$get('ms.mode')$addAllowedValue('neg',
                                                                       'POS'))
    biodb$getEntryFields()$get('ms.mode')$addAllowedValue('pos', 'ZZZ')

    # Create connector
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(db.df)
    
    # Try M/Z matching
    mz.tol <- 5
    mz.tol.unit <- 'ppm'
    mz <- db.df[['peak.mztheo']]
    results <- conn$searchMsPeaks(mz, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit,
                                  ms.mode='pos')
    expect_is(results, 'data.frame')
    results <- conn$searchMsPeaks(mz, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit,
                                  ms.mode='zzz')
    expect_is(results, 'data.frame')
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

test.mass.csv.file.cache.id <- function(biodb) {

    # Open a connector with no URL
    conn <- biodb$getFactory()$createConn('mass.csv.file')

    # Test that cache ID is NULL (in memory database)
    testthat::expect_null(conn$getCacheId())

    # Set URL
    db.file <- file.path(biodb::getTestOutputDir(), 'test.mass.csv.file.cache.id_db.tsv')
    conn$setUrl('base.url', db.file)

    # Test that cache ID is not NULL
    testthat::expect_is(conn$getCacheId(), 'character')
    testthat::expect_true(nchar(conn$getCacheId()) > 0)
    cache.id <- conn$getCacheId()

    # Close the connector
    biodb$getFactory()$deleteConn(conn$getId())

    # Open a connector to the same URL
    conn <- biodb$getFactory()$createConn('mass.csv.file', url=db.file)

    # Test that we get the same cache ID
    testthat::expect_equal(conn$getCacheId(), cache.id)

    # Close the connector
    biodb$getFactory()$deleteConn(conn$getId())
}

test.mass.csv.file.cache.confusion <- function(biodb) {

    # Create data frame for new file db A
    entry.id <- 'K'
    df <- rbind(data.frame(accession=entry.id, ms.mode='pos', ms.level=1,
                           peak.mztheo=219.1127765, peak.intensity=373076,
                           peak.relative.intensity=999),
                stringsAsFactors=FALSE)

    # Open a connector to data frame and set URL to file db A
    db.A.file <- file.path(biodb::getTestOutputDir(),
                           'test.mass.csv.file.cache.confusion_db_A.tsv')
    if (file.exists(db.A.file))
        unlink(db.A.file)
    conn <- biodb$getFactory()$createConn('mass.csv.file', url=db.A.file)
    conn$setDb(df)

    # Save database
    conn$allowWriting()
    conn$write()

    # Get entry with ID K
    entry <- conn$getEntry(entry.id)

    # Close connector
    biodb$getFactory()$deleteConn(conn$getId())

    # Open a connector to a file db B that does not exist
    db.B.file <- file.path(biodb::getTestOutputDir(), 'test.mass.csv.file.cache.confusion_db_B.tsv')
    if (file.exists(db.B.file))
        unlink(db.B.file)
    conn <- biodb$getFactory()$createConn('mass.csv.file', url = db.B.file)
    conn$allowWriting()

    # Get entry with the same ID K
    entry <- conn$getEntry(entry.id)

    # Check that no entry is returned
    testthat::expect_null(entry)

    # Close connector to file db B
    biodb$getFactory()$deleteConn(conn$getId())
}

test_massCsvFile_entry_instantiation <- function(biodb) {

    # Create database
    db <- data.frame(
        accession=c("A1", "A1", "A1", "A1", "A1"),
        peak.mztheo=c(80, 90, 100, 110, 120),
        msprecmz=90,
        stringsAsFactors=FALSE)

    # Create connector
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(db)

    # Get entry
    entry <- conn$getEntry("A1")
    testthat::expect_is(entry, "BiodbEntry")

    # Test
    x <- entry$getFieldsAsDataframe(fields='accession')
    expX <- data.frame(accession='A1', stringsAsFactors=FALSE)
    testthat::expect_identical(x, expX)

    # Delete connector
    biodb$getFactory()$deleteConn(conn$getId())
}

test_msmsSearch_R4.0_non_regression <- function(biodb) {

    db.tsv <- system.file("extdata", "massbank_extract_msms.tsv",
                          package='biodb')
    conn <- biodb$getFactory()$createConn('mass.csv.file', url=db.tsv)
    testthat::expect_is(conn, 'BiodbConn')
    spectrum <- data.frame(mz=c(286.1456, 287.1488, 288.1514),
                           rel.int=c(100, 45, 18))
    result <- conn$msmsSearch(spectrum, precursor.mz=286.1438, mz.tol=0.1,
                              mz.tol.unit='plain', ms.mode='pos')
    testthat::expect_s3_class(result, 'data.frame')
}

test_matchingField <- function(biodb) {

    # Database with no M/Z field
    df <- data.frame(accession='1')
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(df)
    testthat::expect_error(conn$getMatchingMzField())
    
    # Database with peak.mztheo only
    mz1 <- 132.12
    df <- data.frame(accession='1', peak.mztheo=mz1)
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(df)
    testthat::expect_equal(conn$getMatchingMzField(), 'peak.mztheo')
    testthat::expect_equal(conn$searchForMassSpectra(mz=mz1, mz.tol=0), '1')
    testthat::expect_equal(conn$searchForMassSpectra(mz=mz1+1, mz.tol=0),
                           character())
    
    # Database with peak.mzexp only
    df <- data.frame(accession='1', peak.mzexp=mz1)
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(df)
    testthat::expect_equal(conn$getMatchingMzField(), 'peak.mzexp')
    testthat::expect_equal(conn$searchForMassSpectra(mz=mz1, mz.tol=0), '1')
    testthat::expect_equal(conn$searchForMassSpectra(mz=mz1+1, mz.tol=0),
                           character())
    
    # Database with both peak.mztheo and peak.mzexp
    mz2 <- 132.1210
    mz3 <- 132.1243
    df <- data.frame(accession='1', peak.mztheo=mz2, peak.mzexp=mz3)
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(df)
    testthat::expect_warning(field <- conn$getMatchingMzField())
    testthat::expect_equal(field, 'peak.mztheo')
    testthat::expect_equal(conn$searchForMassSpectra(mz=mz2, mz.tol=0), '1')
    testthat::expect_equal(conn$searchForMassSpectra(mz=mz2+1, mz.tol=0),
                           character())
    testthat::expect_equal(conn$searchForMassSpectra(mz=mz3, mz.tol=0),
                           character())
    conn$setMatchingMzField('peak.mzexp')
    testthat::expect_equal(conn$getMatchingMzField(), 'peak.mzexp')
    testthat::expect_equal(conn$searchForMassSpectra(mz=mz2, mz.tol=0),
                           character())
    testthat::expect_equal(conn$searchForMassSpectra(mz=mz3, mz.tol=0), '1')
    testthat::expect_equal(conn$searchForMassSpectra(mz=mz3+1, mz.tol=0),
                           character())
}

# MAIN

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Set context
biodb::testContext("Test Mass spectra CSV File connector.")

# Create connector
conn <- biodb$getFactory()$createConn('mass.csv.file')
conn$setUrl('base.url', MASSFILEDB.URL)

# Make sure we have no residual cache entries from previous tests
biodb$getPersistentCache()$deleteAllFiles(conn$getCacheId(), fail=FALSE)

# Run tests
biodb::runGenericTests(conn)
biodb::testThat('Test fields manipulation works correctly.', test.fields,
                conn=conn)
biodb::testThat("Mass CSV file entry construction works.",
                test_massCsvFile_entry_instantiation, biodb=biodb)
biodb::testThat("MassCsvFileConn methods are correct",
                test.basic.mass.csv.file, conn=conn)
biodb::testThat("M/Z match output contains all columns of database.",
                test.mass.csv.file.output.columns, conn=conn)
biodb::testThat('Test that we detect undefined fields', test.undefined.fields,
                biodb=biodb)
biodb::testThat('Setting database with a data frame works.',
                test.mass.csv.file.data.frame, biodb=biodb)
biodb::testThat('Failure occurs when loading database file with a line containing wrong number of values.', test.wrong.nb.cols, biodb=biodb)
biodb::testThat('Failure occurs when a field with a cardinality of one has several values for the same accession number.', test.field.card.one, biodb=biodb)
biodb::testThat('We can search for precursor M/Z values without peak.attr column defined.', test.getMzValues.without.peak.attr, biodb=biodb)
biodb::testThat('Old column names are still recognized.',
                test.mass.csv.file.old.col.names, biodb=biodb)
biodb::testThat('Peaks table of entry has official field names.',
                test.mass.csv.file.entry.peaks.table.col.names, biodb=biodb)
biodb::testThat('M/Z matching limits (mz.min and mz.max) are respected.',
                test.mass.csv.file.mz.matching.limits, biodb=biodb)
biodb::testThat('RT matching limits (rt.min and rt.max) are respected.',
                test.mass.csv.file.rt.matching.limits, biodb=biodb)
biodb::testThat('We can set additional values for MS mode.',
                test.mass.csv.file.ms.mode.values, biodb=biodb)
biodb::testThat('Precursor match works.', test.mass.csv.file.precursor.match,
                biodb=biodb)
biodb::testThat('Two different databases do not use the same cache files.',
                test.mass.csv.file.cache.confusion, biodb=biodb)
biodb::testThat('Sorting of columns of result frame works well in searchMsPeaks().', test.mass.csv.file.searchMsPeaks.column.sorting, biodb=biodb)
biodb::testThat('Cache ID is set correctly.', test.mass.csv.file.cache.id,
                biodb=biodb)
biodb::testThat('R 4.0 error "NA\'s are not allowed in y !" with msmsSearch()
                call does not come back.', test_msmsSearch_R4.0_non_regression,
                biodb=biodb)
biodb::testThat("We can defined the M/Z matching field.", test_matchingField,
                biodb=biodb)

# Terminate Biodb
biodb$terminate()
