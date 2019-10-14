# vi: fdm=marker

# Constants {{{1
################################################################

ENV <- Sys.getenv()

TEST.DIR <- file.path(getwd(), '..')
OUTPUT.DIR <- file.path(TEST.DIR, 'output')
RES.DIR  <- file.path(TEST.DIR, 'res')
REF.FILES.DIR <- file.path(RES.DIR, 'ref-files')

MASSFILEDB.URL <- file.path(RES.DIR, 'mass.csv.file.tsv')
MASSFILEDB.WRONG.HEADER.URL <- file.path(RES.DIR, 'mass.csv.file-wrong_header.tsv')
MASSFILEDB.WRONG.NB.COLS.URL <- file.path(RES.DIR, 'mass.csv.file-wrong_nb_cols.tsv')

MASS.SQLITE.URL = file.path(OUTPUT.DIR, 'mass.sqlite.file.sqlite')

# Create output directory {{{1
################################################################

if ( ! file.exists(OUTPUT.DIR))
	dir.create(OUTPUT.DIR)

# Set databases to test {{{1
################################################################

DATABASES.ALL <- 'all'
DATABASES.NONE <- 'none'

# Instantiate biodb
tmpbiodb <- biodb::Biodb()
dbinf <- tmpbiodb$getDbsInfo()

# List databases to test
TEST.DATABASES <- dbinf$getIds()
if ('DATABASES' %in% names(ENV) && nchar(ENV[['DATABASES']]) > 0) {
	if (tolower(ENV[['DATABASES']]) == DATABASES.NONE)
		TEST.DATABASES <- character(0)
	else if (tolower(ENV[['DATABASES']]) == DATABASES.ALL)
		TEST.DATABASES <- dbinf$getIds()
	else {
		TEST.DATABASES <- strsplit(ENV[['DATABASES']], ',')[[1]]
        
        # Check that databases exist
		db.exists <- vapply(TEST.DATABASES, function(x) dbinf$isDefined(x), FUN.VALUE = TRUE)
		if ( ! all(db.exists)) {
			wrong.dbs <- TEST.DATABASES[ ! db.exists]
			stop(paste0('Cannot run tests, the following database(s) is/are unknown: ', paste(wrong.dbs, collapse = ', ')), '.')
		}
        
        # Check that databases are enabled
		db.disabled <- vapply(TEST.DATABASES, function(x) dbinf$get(x)$getPropertyValue('disabled'), FUN.VALUE = TRUE)
		if (any(db.disabled)) {
			wrong.dbs <- TEST.DATABASES[db.disabled]
			stop(paste0('Cannot run tests, the following database(s) is/are disabled: ', paste(wrong.dbs, collapse = ', ')), '.')
		}
	}
}

# Do not test disabled databases
fct <- function(x) dbinf$get(x)$getPropertyValue('disabled')
to_remove <- vapply(TEST.DATABASES, fct, FUN.VALUE=TRUE)
TEST.DATABASES <- TEST.DATABASES[ ! to_remove]

# Terminate biodb instance
tmpbiodb$terminate()
tmpbiodb <- NULL
dbinf <- NULL

# List reference entries {{{1
################################################################

list.ref.entries <- function(db) {

	# List json files
	files <- Sys.glob(file.path(RES.DIR, paste('entry', db, '*.json', sep = '-')))
	if (length(files) == 0)
		stop(paste0("No JSON reference files for database ", db, "."))

	# Extract ids
	ids <- sub(paste('^.*/entry', db, '(.+)\\.json$', sep = '-'), '\\1', files, perl = TRUE)

	# Replace encoded special characters
	ids = gsub('%3a', ':', ids)

	return(ids)
}

# Load ref entry {{{1
################################################################

load.ref.entry <- function(db, id) {

	# Replace forbidden characters
	id = gsub(':', '%3a', id)

	# Entry file
	file <- file.path(RES.DIR, paste('entry-', db, '-', id, '.json', sep = ''))
	expect_true(file.exists(file), info = paste0('Cannot find file "', file, '" for ', db, ' reference entry', id, '.'))

	# Load JSON
	json <- jsonlite::fromJSON(file)

	# Set NA values
	for (n in names(json))
		if (length(json[[n]]) == 1) {
			if (json[[n]] == 'NA_character_')
				json[[n]] <- NA_character_
		}

	return(json)
}

# Load reference entries {{{1
################################################################

load.ref.entries <- function(db) {

	entries.desc <- NULL

	# List JSON files
	entry.json.files <- Sys.glob(file.path(RES.DIR, paste('entry', db, '*.json', sep = '-')))

	# Loop on all JSON files
	for (f in entry.json.files) {

		# Load entry from JSON
		entry <- jsonlite::read_json(f)

		# Replace NULL values by NA
		entry <- lapply(entry, function(x) if (is.null(x)) NA else x)

		# Convert to data frame
		entry.df <- as.data.frame(entry, stringsAsFactors = FALSE)

		# Append entry to main data frame
		entries.desc <- plyr::rbind.fill(entries.desc, entry.df)
	}

	return(entries.desc)
}


# Create connector for generic tests {{{1
################################################################

create.conn.for.generic.tests = function(biodb, class.db) {

	# Get connector
	if (biodb$getFactory()$connExists(class.db))
		conn = biodb$getFactory()$getConn(class.db)

	# Create connector
	else {
		conn = biodb$getFactory()$createConn(class.db)

		# Set parameters for local connectors
		if (class.db == 'mass.csv.file') {
			conn$setUrl('base.url', MASSFILEDB.URL)
			conn$setField('accession', c('compound.id', 'ms.mode', 'chrom.col.name', 'chrom.rt'))
			biodb$getCache()$deleteAllFiles(conn$getCacheId()) # Make sure we have no residual cache entries from previous tests
		}
		else if (class.db == 'mass.sqlite') {
			conn$setUrl('base.url', MASS.SQLITE.URL)
			biodb$getCache()$deleteAllFiles(conn$getCacheId()) # Make sure we have no residual cache entries from previous tests

			# Create SQLite database file
			if ( ! file.exists(MASS.SQLITE.URL)) {

				mass.csv.file.conn = create.conn.for.generic.tests(biodb = biodb, class.db = 'mass.csv.file')
				conn$allowEditing()
				biodb$copyDb(conn.from = mass.csv.file.conn, conn.to = conn)
				conn$allowWriting()
				conn$write()
				conn$disallowWriting()
				conn$disallowEditing()
			}
		}

		# Create needed additional connectors for computing missing fields
		for (field in biodb$getEntryFields()$getFieldNames())
			for (computing.db in biodb$getEntryFields()$get(field)$getComputableFrom())
				if (computing.db != class.db)
					c = create.conn.for.generic.tests(biodb = biodb, class.db = computing.db)
	}

	return(conn)
}

