test_cache_for_local_db <- function(conn, opt) {
    bdb <- conn$getBiodb()
    bdb$getConfig()$set('use.cache.for.local.db', TRUE)
    entries <- opt$refEntries$getRealEntries()
    testthat::expect_is(entries, 'list')
    testthat::expect_true(length(entries) > 0)
    testthat::expect_false(any(vapply(entries, is.null, FUN.VALUE=FALSE)))
    bdb$getConfig()$set('use.cache.for.local.db', FALSE)
}

checkEntryIds <- function(e, db.name, id, db.id.field) {

    chk::chk_is(e, 'BiodbEntry')

    # Check IDs
    testthat::expect_true(e$hasField('accession'),
        info=paste0(db.name, ' entry ', id, ' has no accession number.'))
    testthat::expect_true(e$hasField(db.id.field),
        info=paste0(db.name, ' entry ', id, ' has no field ',
        db.id.field, '.'))
    testthat::expect_equal(id, e$getFieldValue('accession'),
        info=paste0(db.name, ' entry ', id, ' has an accession number (',
        e$getFieldValue('accession'), ') different from the ID.'))
    testthat::expect_equal(e$getFieldValue('accession'),
        e$getFieldValue(db.id.field), info=paste0(db.name, ' entry ', id,
        ' has a value (', e$getFieldValue(db.id.field),
        ') of database id field (', db.id.field,
        ') different from the accession number (',
        e$getFieldValue('accession'), ').'))
}

checkEntryFields <- function(e, ref.entry, id, db.name, ef, db.id.field) {

    # Loop on all reference fields
    for (f in names(ref.entry)) {
        v <- ref.entry[[f]]
        w <- e$getFieldValue(f)
        if (is.data.frame(v))
            v <- as.data.frame(v, stringsAsFactors=FALSE)

        # Check that field exists
        testthat::expect_true(ef$get(f)$isVirtual() ||
            e$hasField(f), info=paste0('Field "', f,
            '" cannot be found inside ', db.name, ' entry ', id, '.'))

        # Check field type
        testthat::expect_equal(typeof(w), typeof(v),
            info=paste0('Type of field "', f, '" for database ', db.name,
            ' entry ', id, ' (', typeof(w),
            ') is different in reference entry (', typeof(v), ').'))

        # Check length
        testthat::expect_equal(length(w), length(v),
            info=paste0('Length of field "', f, '" for database ', db.name,
            ' entry ', id, ' (', length(w),
            ') is different in reference entry (', length(v), ').'))
        if ( ! is.vector(v) || length(v) < 20 || length(v) != length(w))
            testthat::expect_identical(w, v, info=paste0('Value of field "',
            f, '" for database ', db.name, ' entry ', id,
            ' (', lst2str(w),
            ') is different in reference entry (',
            lst2str(v), ').'))
        else
            testthat::expect_identical(w, v, info=paste0('Value of field "',
            f, '" for database ', db.name, ' entry ', id,
            ' is different in reference entry. Non equal values are: ',
            paste(vapply(which(v != w), function(i) paste(w[[i]], '!=',
            v[[i]]), FUN.VALUE=''), collapse=', '), '.'))
    }

    # Loop on all fields of loaded entry
    for (f in e$getFieldNames())
        if ( ! f %in% c(db.id.field, 'peaks'))
            testthat::expect_true(any(ef$get(f)$getAllNames() %in%
            names(ref.entry)), info=paste0('Field ', f, ' of ', db.name,
            ' entry ', id, ' has not been tested. Its value is: ',
            paste(e$getFieldValue(f), collapse=', '), '.'))
}

test.entry.fields <- function(conn, opt) {

    biodb <- conn$getBiodb()
    db.name <- conn$getId()
    db.id.field <- biodb$getDbsInfo()$get(db.name)$getEntryIdField()

    # Get reference entries
    ref.ids <- opt$refEntries$getAllIds(limit=opt$maxRefEntries)

    # Loop on all entries
    for (id in ref.ids) {
        content <- opt$refEntries$getContents(id)
        e <- biodb$getFactory()$createEntryFromContent(conn$getId(),
            content=content)
        opt$refEntries$saveEntriesAsJson(id, list(e))
        checkEntryIds(e, db.name=db.name, id=id, db.id.field=db.id.field)
        checkEntryFields(e, ref.entry=opt$refEntries$getRefEntry(id), id=id,
            db.name=db.name, ef=biodb$getEntryFields(),
            db.id.field=db.id.field)
    }
}

testEntryLoading <- function(conn, opt) {
    id <- opt$refEntries$getAllIds(limit=1)
    testthat::expect_is(id, 'character')
    e <- opt$refEntries$getRealEntry(id)
    testthat::expect_is(e, 'BiodbEntry')
}

test.wrong.entry <- function(conn) {

    biodb <- conn$getBiodb()
    db.name <- conn$getId()

    # Test a wrong accession number
    wrong.entry <- biodb$getFactory()$getEntry(db.name, id = 'WRONGA')
    testthat::expect_null(wrong.entry)
}

test.wrong.entry.among.good.ones <- function(conn, opt) {

    biodb <- conn$getBiodb()
    db.name <- conn$getId()

    # Load reference entries
    entries.desc <- opt$refEntries$getAllRefEntriesDf()

    # Test a wrong accession number
    ids <- c('WRONGB', entries.desc[['accession']])
    logDebug("IDS: %s", lst2str(ids))
    entries <- biodb$getFactory()$getEntry(db.name, id=ids)
    logDebug("Entries: %s", lst2str(entries))
    testthat::expect_equal(length(entries), nrow(entries.desc) + 1,
    info=paste0("Error while retrieving entries. ", length(entries),
        " entrie(s) obtained instead of ", nrow(entries.desc) + 1, "."))
    testthat::expect_null(entries[[1]])
    testthat::expect_false(any(vapply(entries[2:length(entries)], is.null,
        FUN.VALUE=TRUE)))
}

test.peak.table <- function(conn, opt) {

    biodb <- conn$getBiodb()
    db.name <- conn$getId()

    # Load reference entries
    entries.desc <- opt$refEntries$getAllRefEntriesDf()

    # Create entries
    entries <- biodb$getFactory()$getEntry(db.name,
        id=entries.desc[['accession']], drop=FALSE)
    testthat::expect_false(any(vapply(entries, is.null, FUN.VALUE=TRUE)),
        "One of the entries is NULL.")
    testthat::expect_equal(length(entries), nrow(entries.desc),
        info=paste0("Error while retrieving entries. ", length(entries),
        " entrie(s) obtained instead of ", nrow(entries.desc), "."))

    # Check number of peaks
    if ('nbpeaks' %in% colnames(entries.desc)) {

        # Check that the registered number of peaks is correct
        testthat::expect_equal(vapply(entries,
            function(e) e$getFieldValue('nbpeaks'), FUN.VALUE=10),
            entries.desc[['nbpeaks']])

        # Check that the peak table has this number of peaks
        peak.tables <- lapply(entries, function(e) e$getFieldValue('peaks'))
        testthat::expect_false(any(vapply(peak.tables, is.null,
            FUN.VALUE=TRUE)))
        testthat::expect_equal(entries.desc[['nbpeaks']],
            vapply(peak.tables, nrow, FUN.VALUE=1))

        # Check that the peak table contains the right columns
        # TODO
    }
}

test.nb.entries <- function(conn) {

    # Test getNbEntries()
    n <- conn$getNbEntries()
    testthat::expect_true(is.na(n) || n >= 0)
}

test.entry.ids <- function(conn) {

    # Test getEntryIds()
    max <- 100
    ids <- conn$getEntryIds(max.results = max)
    testthat::expect_is(ids, 'character')
    testthat::expect_true(length(ids) <= max)
}

test.rt.unit <- function(conn, opt) {

    # Get IDs of reference entries
    ref.ids <- opt$refEntries$getAllIds(limit=opt$maxRefEntries)

    # Get entries
    entries <- conn$getBiodb()$getFactory()$getEntry(conn$getId(), id=ref.ids,
        drop=FALSE)

    # Loop on all entries
    for (e in entries)
        testthat::expect_true( ( ! e$hasField('chrom.rt') &&
            ! e$hasField('chrom.rt.min') && ! e$hasField('chrom.rt.max')) ||
            e$hasField('chrom.rt.unit'), paste0('No RT unit for entry ',
            e$getFieldValue('accession'), '. If an entry defines a retention',
            ' time, it must also defines the unit.'))
}

test.entry.page.url <- function(conn, opt) {

    # Get IDs of reference entries
    ref.ids <- opt$refEntries$getAllIds(limit=opt$maxRefEntries)

    # Get URLs
    urls <- conn$getEntryPageUrl(ref.ids)

    # Check
    testthat::expect_is(urls, 'character')
    testthat::expect_length(urls, length(ref.ids))
}

test.entry.image.url <- function(conn, opt) {

    # Get IDs of reference entries
    ref.ids <- opt$refEntries$getAllIds(limit=opt$maxRefEntries)

    # Get URLs
    urls <- conn$getEntryImageUrl(ref.ids)

    # Check
    testthat::expect_is(urls, 'character')
    testthat::expect_length(urls, length(ref.ids))
}

test.entry.page.url.download <- function(conn, opt) {

    # Get IDs of reference entries
    ref.ids <- opt$refEntries$getAllIds()

    # Get URL
    url <- conn$getEntryPageUrl(ref.ids[[1]])
    testthat::expect_is(url, 'character')

    # Try downloading
    if ( ! is.na(url)) {
        biodb::logDebug('Trying to download "%s".', url)
        content <- getUrlContent(url)
        testthat::expect_true( ! is.na(content))
        testthat::expect_true(nchar(content) > 0)
        testthat::expect_length(grep('<title>.*Not Found</title>', content), 0)
    }
}

test.entry.image.url.download <- function(conn, opt) {

    # Get IDs of reference entries
    ref.ids <- opt$refEntries$getAllIds(limit=opt$maxRefEntries)

    # Get URL
    url <- conn$getEntryImageUrl(ref.ids[[1]])
    testthat::expect_is(url, 'character')

    # Try downloading
    if ( ! is.na(url)) {
        content <- getUrlContent(url, binary=TRUE)
        testthat::expect_is(content, 'raw')
    }
}

test.create.conn.with.same.url <- function(conn) {
    testthat::expect_error(
        conn$getBiodb()$getFactory()$createConn(conn$getDbClass(),
        url=conn$getUrl('base.url')))
}

test.db.editing <- function(conn) {

    # Get one entry from connector
    id = conn$getEntryIds(1)
    entry = conn$getEntry(id)
    testthat::expect_is(entry, 'BiodbEntry')

    # Create other connector
    conn.2 = conn$getBiodb()$getFactory()$createConn(conn$getDbClass())
    conn.2$allowEditing()
    conn.2$addNewEntry(entry$cloneInstance())

    # Test methods
    id = conn.2$getEntryIds()
    testthat::expect_length(id, 1)
    testthat::expect_equal(id, entry$getFieldValue('accession'))

    # Delete connector
    conn.2$getBiodb()$getFactory()$deleteConn(conn.2$getId())
}

test.db.writing.with.col.add <- function(conn) {

    # Set database file
    db.file <- file.path(getTestOutputDir(),
        paste('test.db.writing.with.col.add',
        conn$getDbClass(), 'db', sep='.'))
    if (file.exists(db.file))
        unlink(db.file)

    # Get one entry from connector
    id = conn$getEntryIds(1)
    entry = conn$getEntry(id)
    testthat::expect_is(entry, 'BiodbEntry')

    # Create other connector
    conn.2 = conn$getBiodb()$getFactory()$createConn(conn$getDbClass(),
        url=db.file)
    conn.2$allowEditing()
    conn.2$allowWriting()
    conn.2$addNewEntry(entry$cloneInstance())

    # Get data frame of all entries
    id = conn.2$getEntryIds()
    testthat::expect_length(id, 1)
    testthat::expect_equal(id, entry$getFieldValue('accession'))
    entries = conn.2$getEntry(id, drop=FALSE)
    entries.df = conn.2$getBiodb()$entriesToDataframe(entries, compute=FALSE,
        only.card.one=TRUE)

    # Get a list of all fields currently used in connector
    current.fields = colnames(entries.df)

    # Choose a field that is not used inside the connector
    fields.def = conn.2$getBiodb()$getEntryFields()
    for (field.name in fields.def$getFieldNames()) {
        field = fields.def$get(field.name)
        if (field$hasCardOne() && field$isVector()
            && ! field.name %in% current.fields)
            break
    }

    # Create a new entry having one new field that does not exist in any other
    # entry (so in the case of SQL the table will have to be altered to add new
    # columns)
    new.entry = entries[[1]]$cloneInstance()
    new.entry$setFieldValue('accession', 'anewentry')
    new.entry$setFieldValue(field$getName(), 0)

    # Add and write the new entry
    conn.2$addNewEntry(new.entry)
    conn.2$write()
    conn.2$getBiodb()$getFactory()$deleteConn(conn.2$getId())
}

test.db.writing <- function(conn) {

    biodb = conn$getBiodb()

    # Get one entry
    entry = conn$getEntry(conn$getEntryIds(1))

    # Set database file
    db.file <- file.path(getTestOutputDir(), paste('test.db.writing',
        conn$getDbClass(), 'db', sep = '.'))
    if (file.exists(db.file))
        unlink(db.file)

    # Create new database
    conn.2 = biodb$getFactory()$createConn(conn$getDbClass())
    testthat::expect_error(conn.2$write())

    # Clone entry
    testthat::expect_true(entry$parentIsAConnector())
    entry.2 <- entry$cloneInstance()
    testthat::expect_false(entry.2$parentIsAConnector())

    # Compare entries
    df.1 <- biodb$entriesToDataframe(list(entry), only.atomic=FALSE,
        sort.cols=TRUE)
    df.2 <- biodb$entriesToDataframe(list(entry.2), only.atomic=FALSE,
        sort.cols=TRUE)
    testthat::expect_identical(df.1, df.2)

    # Add new entry
    testthat::expect_length(conn.2$getAllVolatileCacheEntries(), 0)
    testthat::expect_null(conn.2$getEntry(entry$getId()))
    testthat::expect_length(conn.2$getAllVolatileCacheEntries(), 0)
    testthat::expect_error(conn.2$addNewEntry(entry.2))
    testthat::expect_length(conn.2$getAllVolatileCacheEntries(), 0)
    conn.2$allowEditing()
    conn.2$addNewEntry(entry.2)
    testthat::expect_length(conn.2$getAllVolatileCacheEntries(), 1)
    testthat::expect_true(entry.2$parentIsAConnector())
    testthat::expect_error(conn.2$addNewEntry(entry.2))
    testthat::expect_length(conn.2$getAllVolatileCacheEntries(), 1)

    # Write database
    testthat::expect_true(entry.2$isNew())
    testthat::expect_error(conn.2$write())
    conn.2$setUrl('base.url', db.file)
    testthat::expect_error(conn.2$write())
    conn.2$allowWriting()
    conn.2$write()
    testthat::expect_false(entry.2$isNew())

    # Reload database file
    biodb$getFactory()$deleteConn(conn.2$getId())
    conn.3 = biodb$getFactory()$createConn(conn$getDbClass(), url = db.file)
    entry.3 = conn.3$getEntry(entry$getId())
    testthat::expect_is(entry.3, 'BiodbEntry')
    biodb$getFactory()$deleteConn(conn.3$getId())
}

test.db.copy <- function(conn) {

    biodb <- conn$getBiodb()

    # Set destination database file
    db.file <- file.path(getTestOutputDir(),
        paste('test.db.copy', conn$getDbClass(), 'db', sep='.'))
    if (file.exists(db.file))
        unlink(db.file)

    # Create new connector
    conn.2 <- biodb$getFactory()$createConn(conn$getDbClass(), url=db.file)

    # Copy database
    conn.2$allowEditing()
    conn.2$allowWriting()
    biodb$copyDb(conn.from=conn, conn.t=conn.2)

    # Compare
    ids.1 <- conn$getEntryIds()
    ids.2 <- conn.2$getEntryIds()
    testthat::expect_identical(ids.1, ids.2)
    entries.1 <- conn$getEntry(ids.1)
    entries.2 <- conn.2$getEntry(ids.2)
    df.1 <- biodb$entriesToDataframe(entries.1, only.atomic=FALSE,
        compute=FALSE)
    df.2 <- biodb$entriesToDataframe(entries.2, only.atomic=FALSE,
        compute=FALSE)
    testthat::expect_identical(df.1, df.2)

    # Delete connector
    biodb$getFactory()$deleteConn(conn.2$getId())
}

test.searchForEntries <- function(conn, opt) {
    
    max.results <- 0
    if ( ! is.null(opt) && 'max.results' %in% names(opt))
        max.results <- opt[['max.results']]

    ef <- conn$getBiodb()$getEntryFields()
    fields <- conn$getPropertyValue('searchable.fields')
    testthat::expect_is(fields, 'character')
    if ('skip.searchable.fields' %in% names(opt))
        fields <- fields[ ! fields %in% opt$skip.searchable.fields]

    for (f in fields) {
        
        # Test character field
        if (ef$get(f)$getClass() == 'character') {
            
            # Get an entry
            id <- opt$refEntries$getAllIds(limit=1)
            biodb::logDebug(
                'Testing searchForEntries() with entry "%s" and field "%s".',
                id, f)
            testthat::expect_true( ! is.null(id))
            testthat::expect_true(length(id) == 1)
            entry <- conn$getEntry(id, drop=TRUE)
            testthat::expect_true( ! is.null(entry))

            # Search by field's value
            v <- entry$getFieldValue(f)
            if ( ! is.null(v)) {
                testthat::expect_is(v, 'character')
                testthat::expect_gt(length(v), 0)
                v <- v[[1]]
            }
            biodb::logDebug0('With value "', (if (is.null(v)) 'NULL' else v),
                '".')
            x <- list()
            x[[f]] <- v
            ids <- conn$searchForEntries(fields=x, max.results=max.results)
            biodb::logDebug('With found IDs %s.', lst2str(ids))
            
            # Test result
            if (is.null(v) || is.na(v) || v == '') {
                testthat::expect_is(ids, 'character')
                if (is.null(v) || is.na(v))
                    testthat::expect_true(length(ids) == 0)
            }
            else {
                testthat::expect_true( ! is.null(ids))
                testthat::expect_true(length(ids) > 0)
                testthat::expect_true(!!id %in% !!ids)
            }
        }
    }
}

test.searchByName <- function(conn, opt) {
    
    # Allow running of deprecated methods while testing
    withr::local_options(lifecycle_verbosity="quiet")

    max.results <- 0
    if ( ! is.null(opt) && 'max.results' %in% names(opt))
        max.results <- opt[['max.results']]

    if (conn$isSearchableByField('name')) {

        # Get an entry
        id <- opt$refEntries$getAllIds(limit=1)
        testthat::expect_true( ! is.null(id))
        testthat::expect_length(id, 1)
        entry <- conn$getEntry(id, drop=TRUE)
        testthat::expect_true( ! is.null(entry))

        # Search by name
        name <- entry$getFieldValue('name')
        testthat::expect_is(name, 'character')
        testthat::expect_gt(length(name), 0)
        name <- name[[1]]
        testthat::expect_true( ! is.na(name))
        ids <- conn$searchByName(name=name, max.results=max.results)

        # Test
        msg <- paste0('While searching for entry ', id, ' by name "', name,
            '".')
        testthat::expect_true( ! is.null(ids), msg)
        testthat::expect_true(length(ids) > 0, msg)
        testthat::expect_true(id %in% ids, msg)
    }
    else {
        testthat::expect_warning(conn$searchByName(name='foo'))
        testthat::expect_warning(conn$searchByName(name=''))
        ids <- conn$searchByName(name=NULL)
        testthat::expect_is(ids, 'character')
        testthat::expect_length(ids, 0)
    }
    withr::local_options(lifecycle_verbosity="error")
}

test.searchCompound <- function(db, opt) {
    
    max.results <- 0
    if ( ! is.null(opt) && 'max.results' %in% names(opt))
        max.results <- opt[['max.results']]

    # Get an entry
    id <- opt$refEntries$getAllIds(limit=1)
    testthat::expect_true( ! is.null(id))
    testthat::expect_length(id, 1)
    entry <- db$getEntry(id, drop = TRUE)
    testthat::expect_true( ! is.null(entry))

    # Search by name
    name <- ''
    if (db$isSearchableByField('name')) {
        name <- entry$getFieldValue('name')
        testthat::expect_is(name, 'character')
        testthat::expect_gt(length(name), 0)
        name <- name[[1]]
        testthat::expect_true( ! is.na(name))
    }
    lifecycle::expect_deprecated(db$searchCompound(name=name,
        max.results=max.results))
    withr::local_options(lifecycle_verbosity="quiet")
    if (db$isSearchableByField('name')) {
        ids <- db$searchCompound(name=name, max.results=max.results)
        msg <- paste0('While searching for entry ', id, ' by name "', name,
            '".')
        testthat::expect_true( ! is.null(ids), msg)
        testthat::expect_true(length(ids) > 0, msg)
        testthat::expect_true(id %in% ids, msg)
    } else {
        testthat::expect_warning(ids <- db$searchCompound(name=name,
            max.results=max.results))
        testthat::expect_is(ids, 'character')
        testthat::expect_length(ids, 0)
    }

    # Loop on all mass fields
    for (field in db$getBiodb()$getEntryFields()$getFieldNames(type='mass')) {

        if ( ! entry$hasField(field)) {
            testthat::expect_false(db$isSearchableByField(field),
            paste0('No test for searchCompound() with mass field "', field,
            '" for database "', db$getId(), '".'))
            next
        }

        mass <- if (entry$hasField(field)) entry$getFieldValue(field) else 10.0
        if (field == 'molecular.mass')
            mass.tol <- 0.01
        else if (mass != floor(mass)) {
            n <- as.integer(nchar(strsplit(as.character(mass),
                '\\.')[[1]][[2]]))
            mass.tol <- 10^-n
        } else
            mass.tol <- 1.0

        # Search by mass
        max.results <- 3
        msg <- paste0('While searching for entry ', id, ' by mass ', mass,
            ' with mass field ', field, '.')
        if ( ! db$isSearchableByField(field)) {
            testthat::expect_warning(ids <- db$searchCompound(mass=mass,
                mass.tol=mass.tol, mass.field=field, max.results=max.results))
            testthat::expect_is(ids, 'character')
            testthat::expect_length(ids, 0)
        } else {
            ids <- db$searchCompound(mass=mass, mass.tol=mass.tol,
                mass.field=field, max.results=max.results)
            testthat::expect_true( ! is.null(ids), msg)
            testthat::expect_true(length(ids) > 0, msg)
            # Search again if not found with limited max.results
            if ( ! id %in% ids) {
                max.results <- 1000000
                ids <- db$searchCompound(mass=mass, mass.tol=mass.tol,
                    mass.field=field, max.results=max.results)
            }
            testthat::expect_true(id %in% ids, msg)
            testthat::expect_true(is.na(max.results)
                || length(ids) <= max.results)

            # Search by mass and name
            if (db$isSearchableByField('name')) {

                # Search by mass and name
                ids <- db$searchCompound(name=name, mass=mass,
                    mass.tol=mass.tol, mass.field=field,
                    max.results=max.results)
                msg <- paste0('While searching for entry ', id, ' by mass ',
                    mass, ' with mass field ', field, ' and by name
                    ', name, '.')
                testthat::expect_true( ! is.null(ids), msg)
                testthat::expect_true(length(ids) > 0, msg)
                testthat::expect_true(id %in% ids, msg)
                testthat::expect_true(is.na(max.results)
                    || length(ids) <= max.results)

                # Search by name and slightly different mass
                mass <- mass + mass.tol
                mass.tol <- 2 * mass.tol
                ids <- db$searchCompound(name=name, mass=mass,
                    mass.field=field, mass.tol=mass.tol,
                    max.results=max.results)
                msg <- paste0('While searching for entry ', id, ' by mass ',
                    mass, ' with mass field ', field, ' and by name
                    ', name, '.')
                testthat::expect_true( ! is.null(ids), msg)
                testthat::expect_true(length(ids) > 0, msg)
                testthat::expect_true(id %in% ids, msg)
                testthat::expect_true(is.na(max.results) ||
                    length(ids) <= max.results)
            }
        }
    }
    withr::local_options(lifecycle_verbosity="error")
}

test.annotateMzValues <- function(conn) {

    testthat::expect_null(conn$annotateMzValues(NULL, mz.tol=0.01,
        ms.mode='neg'))
    testthat::expect_identical(data.frame(), conn$annotateMzValues(data.frame(),
        mz.tol=0.01, ms.mode='neg'))
    testthat::expect_identical(data.frame(mz=numeric()),
        conn$annotateMzValues(data.frame(mz=numeric()), mz.tol=0.01,
        ms.mode='neg'))

    # No M/Z column
    testthat::expect_error(conn$annotateMzValues(data.frame(mass=1.0),
        mz.tol=0.01, ms.mode='neg'))

    # Custom M/Z column name
    testthat::expect_identical(data.frame(mass=numeric()),
        conn$annotateMzValues(data.frame(mass=numeric()), mz.tol=0.01,
        ms.mode='neg', mz.col='mass'))

    # An error should be raised if the field name does not exist
    testthat::expect_error(conn$annotateMzValues(data.frame(mz=numeric()),
        mz.tol=0.01, ms.mode='neg', fields='foo'))
}

test.annotateMzValues_real_values <- function(conn, opt) {

    # Mass of a proton
    proton.mass <- 1.0072765

    # Get mass fields
    mass.fields <- conn$getBiodb()$getEntryFields()$getFieldNames('mass')

    # Get entries
    ids <- opt$refEntries$getAllIds(limit=opt$maxRefEntries)
    entries <- conn$getEntry(ids, drop = FALSE)

    # Loop on mass fields
    for (mf in mass.fields) {

        # Get entries that have a value for this mass field
        ewmf <- entries[vapply(entries, function(e) e$hasField(mf),
            FUN.VALUE=TRUE)]
        if (length(ewmf) > 0) {

            # Get masses
            masses <- vapply(ewmf, function(e) as.numeric(e$getFieldValue(mf)),
                FUN.VALUE=1.0)

            # Loop on modes
            for (mode in c('neg', 'pos')) {

                # Compute M/Z values from masses
                mz <- masses + proton.mass * (if (mode == 'neg') -1.0 else +1.0)

                # Loop on mz shifts
                for (shift in c(-0.1, 0.0, 0.1)) {

                    # Create input data frame
                    df <- data.frame(mz=mz+shift)

                    # Test annotate returned value
                    if ( ! conn$isSearchableByField(mf)) {
                        testthat::expect_warning(
                            ret <- conn$annotateMzValues(df,
                                mz.tol=abs(shift)+0.01, mass.field=mf,
                                ms.mode=mode, max.results=3))
                        testthat::expect_identical(ret, df)
                    } else {
                        ret <- conn$annotateMzValues(df,
                            mz.tol=abs(shift)+0.01, mass.field=mf,
                            ms.mode=mode, max.results=3)
                        testthat::expect_is(ret, 'data.frame')
                        testthat::expect_true(all(colnames(df) %in%
                            colnames(ret)))
                        testthat::expect_true(nrow(ret) >= nrow(df))
                        testthat::expect_true(all(df[['mz']] %in% ret[['mz']]))
                        id.col <- conn$getEntryIdField()
                        testthat::expect_true(id.col %in% colnames(ret))
                    }
                }
            }
        }
    }
}

test_annotateMzValues_input_vector <- function(conn, opt) {

    # Mass of a proton
    proton.mass <- 1.0072765

    # Get mass fields
    mass.fields <- conn$getBiodb()$getEntryFields()$getFieldNames('mass')

    # Get entries
    ids <- opt$refEntries$getAllIds(limit=opt$maxRefEntries)
    entries <- conn$getEntry(ids, drop = FALSE)

    # Loop on mass fields
    for (mf in mass.fields) {

        # Get entries that have a value for this mass field
        ewmf <- entries[vapply(entries, function(e) e$hasField(mf),
            FUN.VALUE=TRUE)]
        if (length(ewmf) > 0) {

            # Get masses
            masses <- vapply(ewmf, function(e) as.numeric(e$getFieldValue(mf)),
                FUN.VALUE=1.0)

            # Annotate
            mz <- masses - proton.mass
            if ( ! conn$isSearchableByField(mf)) {
                testthat::expect_warning(
                    ret <- conn$annotateMzValues(mz, mz.tol=0.01,
                        mass.field=mf, ms.mode='neg', max.results=3))
                testthat::expect_identical(ret, data.frame(mz=mz))
            } else {
                ret <- conn$annotateMzValues(mz, mz.tol=0.01, mass.field=mf,
                    ms.mode='neg', max.results=3)
                testthat::expect_is(ret, 'data.frame')
                id.col <- conn$getEntryIdField()
                testthat::expect_identical(c('mz', id.col), colnames(ret))
                testthat::expect_false(any(is.na(ret[[id.col]])))
            }
        }
    }
}

test_annotateMzValues_additional_fields <- function(conn, opt) {

    # Mass of a proton
    proton.mass <- 1.0072765

    # Get mass fields
    mass.fields <- conn$getBiodb()$getEntryFields()$getFieldNames('mass')

    # Get entries
    ids <- opt$refEntries$getAllIds(limit=opt$maxRefEntries)
    entries <- conn$getEntry(ids, drop = FALSE)

    # Loop on mass fields
    for (mf in mass.fields) {

        # Get entries that have a value for this mass field
        ewmf <- entries[vapply(entries, function(e) e$hasField(mf),
            FUN.VALUE=TRUE)]
        if (length(ewmf) > 0) {

            # Get masses
            masses <- vapply(ewmf, function(e) as.numeric(e$getFieldValue(mf)),
                FUN.VALUE=1.0)

            # Annotate
            mz <- masses - proton.mass
            fields <- ewmf[[1]]$getFieldNames()
            outputFields <- paste(conn$getId(), fields, sep='.')
            outputFields <- sub(paste0('^', conn$getId(), '.', conn$getId(),
                '.'), paste0(conn$getId(), '.'), outputFields)
            if ( ! conn$isSearchableByField(mf)) {
                testthat::expect_warning(
                    ret <- conn$annotateMzValues(mz, mz.tol=0.01, mass.field=mf,
                        ms.mode='neg', max.results=3, fields=fields))
                testthat::expect_identical(ret, data.frame(mz=mz))
            } else {
                ret <- conn$annotateMzValues(mz, mz.tol=0.01, mass.field=mf,
                    ms.mode='neg', max.results=3, fields=fields)
                testthat::expect_is(ret, 'data.frame')
                testthat::expect_identical(sort(c('mz', outputFields)),
                    sort(colnames(ret)))
            }

            # We should obtain the same result with a field that does not exist
            # in entries. The field will just be ignored.
            if ( ! conn$isSearchableByField(mf)) {
                testthat::expect_warning(
                    ret <- conn$annotateMzValues(mz, mz.tol=0.01, mass.field=mf,
                        ms.mode='neg', max.results=3, fields=c(fields,
                        'peak.attr')))
                testthat::expect_identical(ret, data.frame(mz=mz))
            } else {
                ret <- conn$annotateMzValues(mz, mz.tol=0.01, mass.field=mf,
                    ms.mode='neg', max.results=3, fields=c(fields,
                    'peak.attr'))
                testthat::expect_is(ret, 'data.frame')
                testthat::expect_identical(sort(c('mz', outputFields)),
                    sort(colnames(ret)))
            }
        }
    }
}

test_annotateMzValues_ppm_tol <- function(conn, opt) {

    # Mass of a proton
    proton.mass <- 1.0072765

    # Get mass fields
    mass.fields <- conn$getBiodb()$getEntryFields()$getFieldNames('mass')

    # Get entries
    ids <- opt$refEntries$getAllIds(limit=opt$maxRefEntries)
    entries <- conn$getEntry(ids, drop = FALSE)

    # Loop on mass fields
    for (mf in mass.fields) {

        # Get entries that have a value for this mass field
        ewmf <- entries[vapply(entries, function(e) e$hasField(mf),
            FUN.VALUE=TRUE)]
        if (length(ewmf) > 0) {

            # Get masses
            masses <- vapply(ewmf, function(e) as.numeric(e$getFieldValue(mf)),
                FUN.VALUE=1.0)

            # Annotate
            mz <- masses - proton.mass
            if ( ! conn$isSearchableByField(mf)) {
                testthat::expect_warning(
                    ret <- conn$annotateMzValues(mz, mz.tol=15,
                        mz.tol.unit='ppm', mass.field=mf, ms.mode='neg',
                        max.results=3))
                testthat::expect_identical(ret, data.frame(mz=mz))
            } else {
                ret <- conn$annotateMzValues(mz, mz.tol=15, mz.tol.unit='ppm',
                    mass.field=mf, ms.mode='neg', max.results=3)
                testthat::expect_is(ret, 'data.frame')
                id.col <- conn$getEntryIdField()
                testthat::expect_identical(c('mz', id.col), colnames(ret))
                testthat::expect_false(any(is.na(ret[[id.col]])))
            }
        }
    }
}

test_annotateMzValues_input_dataframe_untouched <- function(conn, opt) {

    # Mass of a proton
    proton.mass <- 1.0072765

    # Get mass fields
    mass.fields <- conn$getBiodb()$getEntryFields()$getFieldNames('mass')

    # Get entries
    ids <- opt$refEntries$getAllIds(limit=opt$maxRefEntries)
    entries <- conn$getEntry(ids, drop = FALSE)

    # Loop on mass fields
    for (mf in mass.fields) {

        # Get entries that have a value for this mass field
        ewmf <- entries[vapply(entries, function(e) e$hasField(mf),
            FUN.VALUE=TRUE)]
        if (length(ewmf) > 0) {

            # Get masses
            masses <- vapply(ewmf, function(e) as.numeric(e$getFieldValue(mf)),
                FUN.VALUE=1.0)
            testthat::expect_is(masses, 'numeric')

            # Compute M/Z values from masses
            mz <- masses - proton.mass

            # Create input data frame
            idf <- data.frame(mz=mz, col2='a', col3=10L)

            # Test returned value
            if ( ! conn$isSearchableByField(mf)) {
                testthat::expect_warning(
                    ret <- conn$annotateMzValues(idf, mz.tol=0.01,
                        mass.field=mf, ms.mode='neg', max.results=3))
                testthat::expect_identical(ret, idf)
            } else {
                ret <- conn$annotateMzValues(idf, mz.tol=0.01, mass.field=mf,
                    ms.mode='neg', max.results=3)
                testthat::expect_is(ret, 'data.frame')
                testthat::expect_true(all(colnames(idf) %in% colnames(ret)))
                testthat::expect_true(nrow(ret) >= nrow(idf))
                testthat::expect_true(all(idf[['mz']] %in% ret[['mz']]))
                id.col <- conn$getEntryIdField()
                testthat::expect_true(id.col %in% colnames(ret))
                odf <- ret[ ! duplicated(ret[['mz']]), colnames(idf)]
                row.names(idf) <- NULL
                row.names(odf) <- NULL
                testthat::expect_identical(idf, odf)
            }
        }
    }
}

test.msmsSearch.self.match <- function(db) {

    biodb <- db$getBiodb()
    db.name <- db$getId()

    # Set some initial values to speed up test
    db.values <- list(peakforest.mass=list(neg=NULL,
        pos=list(spectrum.id='3828', mz=117.1)),
        massbank=list(neg=list(spectrum.id='PR100504', mz=193.0354),
        pos=list(spectrum.id='AU106501', mz=316.075)))

    # Loop on distance functions
    for (dist.fct in c('wcosine', 'cosine', 'pkernel', 'pbachtttarya'))

        # Loop on modes
        for (mode in c('neg', 'pos')) {

            # Get M/Z value and spectrum ID to be tested
            if (db.name %in% names(db.values) &&
                mode %in% names(db.values[[db.name]])) {
                if (is.null(db.values[[db.name]][[mode]]))
                    next
                mz <- db.values[[db.name]][[mode]]$mz
                spectrum.id <- db.values[[db.name]][[mode]]$spectrum.id
            }
            else {
                # Search for one M/Z value
                mz <- db$getMzValues(ms.mode=mode, ms.level=2, max.results=1,
                    precursor=TRUE)

                # Find corresponding spectrum
                spectrum.id <- db$searchForMassSpectra(mz, mz.tol=5, mz.tol.unit='ppm',
                    ms.mode=mode, max.results=1, ms.level=2, precursor=TRUE)
            }

            # Get entry
            spectrum.entry <- biodb$getFactory()$getEntry(db.name, spectrum.id)

            # Get peaks
            peaks <- spectrum.entry$getFieldValue('peaks')
            int.col <- if ('peak.intensity' %in% names(peaks))
                'peak.intensity' else 'peak.relative.intensity'
            peaks <- peaks[order(peaks[[int.col]], decreasing=TRUE), ]
            peaks <- peaks[seq(2), ]

            # Run MSMS search
            results <- db$msmsSearch(peaks, precursor=mz, mz.tol=0.1,
                mz.tol.unit='plain', ms.mode=mode, npmin=2, dist.fun=dist.fct,
                msms.mz.tol=3, msms.mz.tol.min=0.005)

            # Check results
            testthat::expect_true( ! is.null(results))
            testthat::expect_true(is.data.frame(results))
            testthat::expect_true(nrow(results) > 0)
            cols <- c('id', 'score', paste('peak', seq(nrow(peaks)), sep='.'))
            testthat::expect_true(all(cols %in% colnames(results)))
            testthat::expect_true(spectrum.id %in% results[['id']])
        }
}

test.msmsSearch.empty.spectrum <- function(db) {

    # Define spectrum to match:
    spectrum <- data.frame(mz = numeric(0), rel.int = numeric(0))

    # Search for match:
    result <- db$msmsSearch(spectrum, precursor.mz = 100, mz.tol = 0.3)

    testthat::expect_true( ! is.null(result))
    testthat::expect_true(is.data.frame(result))
    testthat::expect_true(nrow(result) == 0)
    cols <- c('id', 'score')
    testthat::expect_true(all(cols %in% colnames(result)))
}

test.msmsSearch.null.spectrum <- function(db) {

    # Search for match:
    result <- db$msmsSearch(NULL, precursor.mz = 100, mz.tol = 0.3)

    testthat::expect_true( ! is.null(result))
    testthat::expect_true(is.data.frame(result))
    testthat::expect_true(nrow(result) == 0)
    cols <- c('id', 'score')
    testthat::expect_true(all(cols %in% colnames(result)))
}

test.getMzValues <- function(db) {
    max <- 10
    for (mode in c('neg', 'pos')) {
        mz <- db$getMzValues(ms.mode=mode, max.results=max)
        testthat::expect_true(is.double(mz))
        n <- length(mz)
        testthat::expect_true(n >= 1 && n <= max)
    }
}

test.searchForMassSpectra <- function(conn) {

    # Get M/Z values from database
    mode <-'pos' 
    mzs <- conn$getMzValues(ms.mode=mode, max.results=2)
    testthat::expect_vector(mzs, double())
    testthat::expect_true(length(mzs) >= 1)

    # Search
    for (mz in mzs) {
        ids <- conn$searchForMassSpectra(mz=mz, mz.tol=5, mz.tol.unit='plain',
            min.rel.int=0, ms.mode=mode)
        testthat::expect_is(ids, 'character')
        testthat::expect_true(length(ids) > 0)
    }
}

test.searchForMassSpectra.with.NA.value <- function(db) {

    ids <- db$searchForMassSpectra(mz=NA_real_, mz.tol=5.0, mz.tol.unit='plain',
        min.rel.int=0, ms.mode='pos')
    testthat::expect_is(ids, 'character')
    testthat::expect_length(ids, 0)
}

test.searchMsPeaks.with.NA.value <- function(db) {

    # With only one single N/A value
    peaks <- db$searchMsPeaks(mz=NA_real_, mz.tol=5.0, mz.tol.unit='plain')
    testthat::expect_is(peaks, 'data.frame')
    testthat::expect_equal(nrow(peaks), 1)
    testthat::expect_equal(ncol(peaks), 1)
    testthat::expect_equal(colnames(peaks), 'mz')
    testthat::expect_true(is.na(peaks[['mz']]))

    # With one N/A value and three real values
    mode <- 'neg'
    tol <- 0
    mzs <- db$getMzValues(ms.mode=mode, max.results=3)
    mzs <- c(mzs, NA_real_)
    peaks <- db$searchMsPeaks(mz=mzs, mz.tol=tol, mz.tol.unit='plain',
        ms.mode=mode, max.results=2)
    testthat::expect_is(peaks, 'data.frame')
    testthat::expect_true(nrow(peaks) >= length(mzs))
    testthat::expect_true(nrow(peaks) <= 2 * length(mzs))
    testthat::expect_true(ncol(peaks) > 1)
    testthat::expect_false(any(is.na(peaks[seq(nrow(peaks) - 1),
        c('mz', 'accession')])))
    testthat::expect_true(all(is.na(peaks[nrow(peaks), ])))
}

test.searchForMassSpectra.multiple.mz <- function(db) {

    mz.tol <- 0.0001

    # Get M/Z values from database
    mode <- 'pos'
    mzs <- db$getMzValues(ms.mode=mode, max.results=2)
    testthat::expect_is(mzs, 'numeric')
    testthat::expect_true(length(mzs) >= 1)

    # Search one M/Z at a time
    all.ids <- character(0)
    for (mz in mzs) {
        ids <- db$searchForMassSpectra(mz=mz, mz.tol=mz.tol, mz.tol.unit='plain',
            min.rel.int=0, ms.mode=mode)
        testthat::expect_is(ids, 'character')
        testthat::expect_true(length(ids) > 0)
        all.ids <- c(all.ids, ids)
    }
    all.ids <- all.ids[ ! duplicated(all.ids)]

    # Search all M/Z values at once
    all.ids.2 <- db$searchForMassSpectra(mz=mzs, mz.tol=mz.tol,
        mz.tol.unit='plain', min.rel.int=0, ms.mode=mode)

    # List of IDs must be the same
    testthat::expect_true(all(all.ids.2 %in% all.ids))
}

test.searchForMassSpectra.with.precursor <- function(db) {

    biodb <- db$getBiodb()
    db.name <- db$getId()

    # Set some initial values to speed up test
    db.values <- list(massbank=list('1'=list(mz=313.3), '2'=list(mz=285.0208)),
        peakforest.mass=list('2'=list(mz=117.1)))

    tol.ppm <- 5

    # Loop on levels
    for (ms.level in c(1, 2)) {

        # Get an M/Z value of a precursor
        if (db.name %in% names(db.values) &&
            as.character(ms.level) %in% names(db.values[[db.name]]))
            mz <- db.values[[db.name]][[as.character(ms.level)]]$mz
        else
            mz <- db$getMzValues(precursor=TRUE, max.results=1,
                ms.level=ms.level)
        testthat::expect_false(is.null(mz))
        testthat::expect_length(mz, 1)
        testthat::expect_false(is.na(mz))

        # Search for it
        spectra.ids <- db$searchForMassSpectra(mz=mz, mz.tol=tol.ppm,
            mz.tol.unit='ppm', precursor=TRUE, ms.level=ms.level)
        testthat::expect_gte(length(spectra.ids), 1)
        testthat::expect_false(any(is.na(spectra.ids)))

        # Get first entry
        for (spectra.id in spectra.ids) {
            entry <- biodb$getFactory()$getEntry(db.name, spectra.id)
            testthat::expect_false(is.null(entry))
            testthat::expect_false(is.na(entry$getFieldValue('ms.level')))
            testthat::expect_equal(entry$getFieldValue('ms.level'), ms.level)
            peaks <- entry$getFieldValue('peaks')
            testthat::expect_false(is.null(peaks))
            testthat::expect_true(is.data.frame(peaks))
            testthat::expect_gt(nrow(peaks), 0)
            testthat::expect_true('peak.mz' %in% colnames(peaks))

            # Check that precursor peak was matched
            testthat::expect_true(entry$hasField('msprecmz'))
            testthat::expect_true(any(abs(entry$getFieldValue('msprecmz') - mz)
                < mz * tol.ppm * 1e-6))
        }
    }
}

test.searchForMassSpectra.with.precursor.and.multiple.inputs <- function(db) {

    # Input values
    mz <- c(82.04819461, 83.01343941)
    mz.tol <- 5
    mz.tol.unit <- 'ppm'
    ms.level <- 0
    ms.mode <- 'pos'

    # Search
    ids <- db$searchForMassSpectra(mz=mz, mz.tol=mz.tol,
        mz.tol.unit=mz.tol.unit, ms.level=ms.level, ms.mode=ms.mode,
        precursor=TRUE)
    testthat::expect_is(ids, 'character')
}

test.getChromCol <- function(conn, opt) {
    ids <- opt$refEntries$getAllIds(limit=opt$maxRefEntries)
    chrom.col <- conn$getChromCol(ids=ids)
    testthat::expect_is(chrom.col, 'data.frame')
    testthat::expect_identical(names(chrom.col), c('id', 'title'))
    testthat::expect_gt(nrow(chrom.col), 0)
}

test.searchMsPeaks <- function(db) {

    mode <- 'neg'
    tol <- 0.1
    mzs <- db$getMzValues(ms.mode=mode, max.results=3)

    # Test with empty list in input
    testthat::expect_null(db$searchMsPeaks(NULL, mz.tol=tol, max.results=1,
        ms.mode=mode))
    testthat::expect_null(db$searchMsPeaks(integer(), mz.tol=tol,
        max.results=1, ms.mode=mode))
    testthat::expect_null(db$searchMsPeaks(numeric(), mz.tol=tol,
        max.results=1, ms.mode=mode))

    # Test with impossible M/Z value to simulate no match
    impossible.value <- 1e10
    results <- db$searchMsPeaks(impossible.value, mz.tol=tol, max.results=1,
        ms.mode=mode, prefix='myprefix.')
    testthat::expect_is(results, 'data.frame')
    testthat::expect_identical(results, data.frame(mz=impossible.value))

    # Get only one result per M/Z value
    results <- db$searchMsPeaks(mzs, mz.tol=tol, max.results=1, ms.mode=mode)
    testthat::expect_is(results, 'data.frame')
    testthat::expect_true(nrow(results) >= length(mzs))
    testthat::expect_true('accession' %in% names(results))
    testthat::expect_true('peak.mz' %in% names(results))
    fct <- function(mz) {
        any((results$peak.mz >= mz - tol) & (results$peak.mz <= mz + tol))
    }
    testthat::expect_true(all(vapply(mzs, fct, FUN.VALUE=TRUE)))

    # Get 2 results per M/Z value
    results <- db$searchMsPeaks(mzs, mz.tol=tol, max.results=2, ms.mode=mode)
    testthat::expect_is(results, 'data.frame')
    testthat::expect_true(nrow(results) >  length(mzs))
    testthat::expect_true('accession' %in% names(results))
    testthat::expect_true('peak.mz' %in% names(results))
    testthat::expect_true(all(vapply(mzs,
        function(mz) any((results$peak.mz >= mz - tol) &
        (results$peak.mz <= mz + tol)), FUN.VALUE = TRUE)))

    # Test insert.input.values
    results <- db$searchMsPeaks(mzs, mz.tol=tol, max.results=2, ms.mode=mode,
        insert.input.values=TRUE)
    testthat::expect_is(results, 'data.frame')
    testthat::expect_true('mz' %in% colnames(results))
    results <- db$searchMsPeaks(input.df=data.frame(mz=mzs), mz.tol=tol,
        max.results=2, ms.mode=mode, insert.input.values=TRUE)
    testthat::expect_is(results, 'data.frame')
    testthat::expect_true('mz' %in% colnames(results))
    some.col = rep('xxx', length(mzs))
    results <- db$searchMsPeaks(input.df=data.frame(mz=mzs, some.col=some.col,
        stringsAsFactors=FALSE), mz.tol=tol, max.results=2, ms.mode=mode,
        insert.input.values=TRUE)
    testthat::expect_is(results, 'data.frame')
    testthat::expect_true('mz' %in% colnames(results))
    testthat::expect_true('some.col' %in% colnames(results))
    testthat::expect_is(results[['some.col']], 'character')
    testthat::expect_true(all(results[['some.col']] == some.col[[1]]))

    # Test insert.input.values with prefix
    results <- db$searchMsPeaks(input.df=data.frame(mz=mzs, some.col=some.col,
        stringsAsFactors=FALSE), mz.tol=tol, max.results=2, ms.mode=mode,
        insert.input.values=TRUE, prefix='myprefix.')
    testthat::expect_is(results, 'data.frame')
    testthat::expect_true('mz' %in% colnames(results))
    testthat::expect_true('some.col' %in% colnames(results))
    testthat::expect_is(results[['some.col']], 'character')
    testthat::expect_true(all(results[['some.col']] == some.col[[1]]))
}

test.collapseResultsDataFrame <- function(db) {

    mode <- 'neg'
    tol <- 0.1
    mzs <- db$getMzValues(ms.mode=mode, max.results=3)

    # Get 2 results per M/Z value
    results <- db$searchMsPeaks(mzs, mz.tol=tol, max.results=2, ms.mode=mode)
    testthat::expect_is(results, 'data.frame')
    testthat::expect_true(nrow(results) >  length(mzs))

    # Get collapsed data frame
    collapsed.results <- db$collapseResultsDataFrame(results)
    testthat::expect_is(collapsed.results, 'data.frame')
    testthat::expect_equal(nrow(collapsed.results), length(mzs))
    testthat::expect_identical(collapsed.results[['mz']], mzs)
}

test.searchMsPeaks.rt <- function(conn, opt) {

    # Get reference entries
    ids <- opt$refEntries$getAllIds(limit=opt$maxRefEntries)
    entry <- conn$getBiodb()$getFactory()$getEntry(conn$getId(), ids[[1]])

    # Set retention time info
    if (entry$hasField('chrom.rt'))
        rt <- entry$getFieldValue('chrom.rt')
    else if (entry$hasField('chrom.rt.min') && entry$hasField('chrom.rt.max'))
        rt <- (entry$getFieldValue('chrom.rt.min') +
            entry$getFieldValue('chrom.rt.max')) / 2
    testthat::expect_is(rt, 'numeric')
    testthat::expect_false(is.na(rt))
    chrom.col.ids <- entry$getFieldValue('chrom.col.id')
    testthat::expect_is(chrom.col.ids, 'character')
    testthat::expect_false(is.na(chrom.col.ids))
    rt.unit <- entry$getFieldValue('chrom.rt.unit')
    testthat::expect_is(rt.unit, 'character')
    testthat::expect_false(is.na(rt.unit))

    # Get peak table
    peaks <- entry$getFieldValue('peaks')
    mz <- peaks[1, 'peak.mz']
    testthat::expect_is(mz, 'numeric')

    # Search for MZ/RT
    mz.tol <- 0
    rt.tol <- 0
    peaks <- conn$searchMsPeaks(mz=mz, chrom.col.ids=chrom.col.ids, rt=rt,
        rt.tol=rt.tol, mz.tol=mz.tol, max.results=1,
        ms.mode=entry$getFieldValue('ms.mode'), rt.unit=rt.unit)
    testthat::expect_is(peaks, 'data.frame')
    testthat::expect_true(nrow(peaks) > 0)
    testthat::expect_true(all((peaks$peak.mz >= mz - mz.tol) &
        (peaks$peak.mz <= mz + mz.tol)))
    testthat::expect_true(all((peaks$chrom.rt >= rt - rt.tol) &
        (peaks$chrom.rt <= rt + rt.tol)))

    # Search for MZ/RT without chrom.col.ids
    peaks <- conn$searchMsPeaks(mz=mz, rt=rt, rt.tol=rt.tol, mz.tol=mz.tol,
        max.results=1, ms.mode=entry$getFieldValue('ms.mode'), rt.unit=rt.unit)
    testthat::expect_is(peaks, 'data.frame')
    testthat::expect_true(nrow(peaks) > 0)
    testthat::expect_true(all((peaks$peak.mz >= mz - mz.tol) &
        (peaks$peak.mz <= mz + mz.tol)))
    testthat::expect_true(all((peaks$chrom.rt >= rt - rt.tol) &
        (peaks$chrom.rt <= rt + rt.tol)))
}

test.msmsSearch.no.ids <- function(conn) {
    tspec <- data.frame(mz=1, int=10000)
    results <- conn$msmsSearch(tspec, precursor.mz=2, mz.tol=0.5,
        mz.tol.unit='plain', ms.mode="pos", msms.mz.tol=10,
        msms.mz.tol.min=0.01, npmin=2)
    testthat::expect_is(results, 'data.frame')
    testthat::expect_equal(nrow(results), 0)
    testthat::expect_true(all(c('id', 'score') %in% names(results)))
}

# Running quick tests
runGenericShortTests <- function(conn, opt) {
    
    runGenericAdjustableTests(conn=conn, opt=opt)

    biodb::testThat("Wrong entry gives NULL", test.wrong.entry, conn=conn)
    biodb::testThat("One wrong entry does not block the retrieval of good ones",
        test.wrong.entry.among.good.ones, conn=conn, opt=opt)
    biodb::testThat("The peak table is correct.", test.peak.table, conn=conn,
        opt=opt)
    biodb::testThat("Nb entries is positive.", test.nb.entries, conn=conn)
    biodb::testThat("We can get a list of entry ids.", test.entry.ids,
        conn=conn)

    if (conn$isEditable()) {
        biodb::testThat('We can edit a database.', test.db.editing, conn=conn)
        if (conn$isWritable()) {
            biodb::testThat(paste("Creating another connector with the same",
                " URL is forbidden."),
                test.create.conn.with.same.url, conn=conn)
            biodb::testThat('Database writing works.', test.db.writing,
                conn=conn)
            biodb::testThat('We can write entries having new fields.',
                test.db.writing.with.col.add, conn=conn)
        }
    }

    if (conn$isCompounddb() && conn$isSearchableByField(field.type='mass')) {
        biodb::testThat('annotateMzValues() works correctly.',
            test.annotateMzValues, conn=conn)
    }

    if (conn$isMassdb()) {
        biodb::testThat("We can retrieve a list of M/Z values.",
            test.getMzValues, conn=conn)
        biodb::testThat("We can match M/Z peaks.", test.searchForMassSpectra,
                        conn=conn)
        biodb::testThat("We can search for spectra with multiple M/Z values.",
            test.searchForMassSpectra.multiple.mz, conn=conn)
        
        # TODO XXX Commented out because of its dependency on particular
        # connectors
        #biodb::testThat("Search by precursor returns at least one match.",
        #    test.searchForMassSpectra.with.precursor, conn=conn)

        biodb::testThat("Search by precursor on multiple mzs does not fail.",
            test.searchForMassSpectra.with.precursor.and.multiple.inputs, conn=conn)
        biodb::testThat("Search for N/A value returns an empty list.",
            test.searchForMassSpectra.with.NA.value, conn=conn)

        biodb::testThat("We can collapse the results from searchMsPeaks().",
            test.collapseResultsDataFrame, conn=conn)

        # TODO XXX Commented out because of its dependency on particular
        # connectors
        #biodb::testThat(paste("MSMS search can find a match for a spectrum".
        #   "from the database itself."), test.msmsSearch.self.match, conn=conn)

        biodb::testThat('MSMS search works for an empty spectrum.',
            test.msmsSearch.empty.spectrum, conn=conn)
        biodb::testThat('MSMS search works for a null spectrum.',
            test.msmsSearch.null.spectrum, conn=conn)
        biodb::testThat('No failure occurs when msmsSearch found no IDs.',
            test.msmsSearch.no.ids, conn=conn)
    }
}

# Run tests whose duration is adjustable using options (i.e.: maxRefEntries)
runGenericAdjustableTests <- function(conn, opt) {

    # Local dbs
    if ( ! conn$isRemotedb()) {
        biodb::testThat("We can use the cache system for a local database.",
            test_cache_for_local_db, conn=conn, opt=opt)
    }

    # General tests
    biodb::testThat("Entry fields have a correct value", test.entry.fields,
        conn=conn, opt=opt)
    biodb::testThat("RT unit is defined when there is an RT value.",
        test.rt.unit, conn=conn, opt=opt)
    biodb::testThat("We can search for an entry by searchable field",
        test.searchForEntries, conn=conn, opt=opt)
    biodb::testThat("We can search for an entry by name.", test.searchByName,
        conn=conn, opt=opt)
    biodb::testThat("We can load an entry from the database.", testEntryLoading,
        conn=conn, opt=opt)
    
    # Remote dbs
    if (conn$isRemotedb()) {
        biodb::testThat("We can get a URL pointing to the entry page.",
            test.entry.page.url, conn=conn, opt=opt)
        biodb::testThat("We can get a URL pointing to the entry image.",
            test.entry.image.url, conn=conn, opt=opt)
        biodb::testThat("The entry page URL can be downloaded.",
            test.entry.page.url.download, conn=conn, opt=opt)
        biodb::testThat("The entry image URL can be downloaded.",
            test.entry.image.url.download, conn=conn, opt=opt)
    }
    
    # Compound db
    if (conn$isCompounddb() && conn$isSearchableByField(field.type='mass')) {
        biodb::testThat('We can search for a compound', test.searchCompound,
            conn=conn, opt=opt)
        biodb::testThat('annotateMzValues() accepts a single vector.',
            test_annotateMzValues_input_vector, conn=conn, opt=opt)
        biodb::testThat('ppm tolerance works in annotateMzValues()',
            test_annotateMzValues_ppm_tol, conn=conn, opt=opt)
        biodb::testThat('Input data frame is not modified by annotateMzValues()'
            , test_annotateMzValues_input_dataframe_untouched, conn=conn,
            opt=opt)
    }
    
    # Mass db
    if (conn$isMassdb()) {
        biodb::testThat("We can retrieve a list of chromatographic columns.",
            test.getChromCol, conn=conn, opt=opt)
        biodb::testThat("We can search for (M/Z, RT) couples, separately.",
            test.searchMsPeaks.rt, conn=conn, opt=opt)
    }
}

# Running tests that take long time
runGenericLongTests <- function(conn, opt) {
    
    opt$maxRefEntries <- 0
    runGenericAdjustableTests(conn=conn, opt=opt)

    # Compound
    if (conn$isCompounddb() && conn$isSearchableByField(field.type='mass')) {
        biodb::testThat('annotateMzValues() works correctly with real values.',
            test.annotateMzValues_real_values, conn=conn, opt=opt)
        biodb::testThat('Additional fields are accepted in annotateMzValues()',
            test_annotateMzValues_additional_fields, conn=conn, opt=opt)
    }

    # Mass
    if (conn$isMassdb()) {
        biodb::testThat("We can search for several M/Z values, separately.",
            test.searchMsPeaks, conn=conn)
        biodb::testThat("Search for peaks with N/A value returns no match.",
            test.searchMsPeaks.with.NA.value, conn=conn)
    }

    # Editable
    if (conn$isEditable()) {
        if (conn$isWritable()) {
            biodb::testThat('Database copy works.', test.db.copy, conn=conn)
        }
    }
}

#' Run generic tests.
#'
#' This function must be used in tests on all connector classes, before any
#' specific tests.
#'
#' @param conn A valid biodb connector.
#' @param pkgName The name of your package.
#' @param testRefFolder The folder where to find test reference files.
#' @param opt A set of options to pass to the test functions.
#' @param short Run short tests.
#' @param long Run long tests.
#' @param maxShortTestRefEntries The maximum number of reference entries to use
#' in short tests.
#' @return Nothing.
#'
#' @examples
#' # Instantiate a Biodb instance for testing
#' biodb <- biodb::createBiodbTestInstance()
#'
#' # Create a connector instance
#' lcmsdb <- system.file("extdata", "massbank_extract.tsv", package="biodb")
#' conn <- biodb$getFactory()$createConn('mass.csv.file', lcmsdb)
#'
#' # Run generic tests
#' \donttest{
#' biodb::runGenericTests(conn)
#' }
#'
#' # Terminate the instance
#' biodb$terminate()
#'
#' @export
runGenericTests <- function(conn, pkgName, testRefFolder=NULL, opt=NULL,
    short=TRUE, long=FALSE, maxShortTestRefEntries=1) {

    # Checks
    chk::chk_flag(short)
    chk::chk_flag(long)
    chk::chk_whole_number(maxShortTestRefEntries)
    testthat::expect_is(conn, 'BiodbConn')
    chk::chk_string(pkgName)
    chk::chk_null_or(testRefFolder, vld=chk::vld_dir)

    # Create ref entries instance
    opt$refEntries <- TestRefEntries$new(conn$getId(), pkgName=pkgName,
        folder=testRefFolder, bdb=conn$getBiodb())

    # Delete cache entries
    conn$getBiodb()$getFactory()$deleteAllEntriesFromVolatileCache(conn$getId())
    
    # Run short tests
    if (short) {
        opt$maxRefEntries <- maxShortTestRefEntries
        runGenericShortTests(conn=conn, opt=opt)
    }
    
    # Run long tests
    if (long)
        runGenericLongTests(conn=conn, opt=opt)

    return(invisible(NULL))
}
