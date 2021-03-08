#' The mother class of all Mass spectra databases.
#'
#' All Mass spectra databases inherit from this class. It thus defines methods
#' specific to mass spectrometry.
#'
#' @seealso Super class \code{\link{BiodbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get connector
#' conn <- mybiodb$getFactory()$createConn('mass.csv.file')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbConn.R
#' @export BiodbMassdbConn
#' @exportClass BiodbMassdbConn
BiodbMassdbConn <- methods::setRefClass("BiodbMassdbConn",
    contains="BiodbConn",

methods=list(

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbMassdbConn')
},

getChromCol=function(ids=NULL) {
    ":\n\nGets a list of chromatographic columns contained in this database.
    \nids: A character vector of entry identifiers (i.e.: accession numbers).
    Used to restrict the set of entries on which to run the algorithm.
    \nReturned value : A data.frame with two columns, one for the ID 'id' and
    another one for the title 'title'.
    "

    .self$.abstractMethod()
},

getMatchingMzField=function() {
    ":\n\nGets the field to use for M/Z matching.
    \nReturned value: The name of the field (one of peak.mztheo or peak.mzexp).
    "
    
    field <- NULL

    # Get value(s) defined in matching.fields property
    fields <- .self$getPropValSlot('matching.fields', 'mz')
    
    # If it contains one value, return it
    if (length(fields) == 1)
        field <- fields
    
    # If it contains no value, throw an error
    else if (length(fields) == 0)
        .self$error("No macthing field defined for M/Z values.",
                    "Use setMatchingMzField() to set one.")
    
    # If it contains more than one value, try to determine which one to use
    else {

        multiple.match <- FALSE

        # Get the parsing expressions and check which field is associated with
        # a parssing expression
        for (f in fields) {
            pars.expr <- .self$getPropValSlot('parsing.expr', f)
            if ( ! is.null(pars.expr) && ! is.na(pars.expr)) {
                if (is.null(field))
                    field <- f
                else {
                    multiple.match <- TRUE
                    break
                }
            }
        }

        # Otherwise get an entry from the database and check what fields it
        # contains
        if (is.null(field)) {
            id <- .self$getEntryIds(max.results=1)
            if (length(id) == 1) {
                entry <- .self$getEntry(id)
                for (f in fields)
                    if (entry$hasField(f)) {
                        if (is.null(field))
                            field <- f
                        else {
                            multiple.match <- TRUE
                            break
                        }
                    }
            }
        }
        
        # No choice made
        if (is.null(field))
            .self$error("Impossible to determine which field to use for",
                        " M/Z matching. Please set the wanted field using",
                        " setMatchingMzField() method, and make sure it is",
                        " defined inside your database.")
        
        # Throw a warning telling which field was chosen for matching and tell
        # to use setMatchingMzField() to set another field if needed
        .self$setMatchingMzField(field)
        if (multiple.match)
            .self$warning('Field "', field, '" has been automatically chosen',
                          ' among several possibilities (',
                          paste(fields, collapse=', '), ') for matching',
                          ' M/Z values. Use setMatchingMzField() method',
                          ' explicitly to avoid this warning in the future.')
    }
    
    return(field)
},

setMatchingMzField=function(field=c('peak.mztheo', 'peak.mzexp')) {
    ":\n\nSets the field to use for M/Z matching.
    \nfield: The field to use for matching.
    \nReturned value: None.
    "
    
    field <- match.arg(field)
    
    .self$setPropValSlot('matching.fields', 'mz', field)
},

getMzValues=function(ms.mode=NA_character_, max.results=NA_integer_,
                     precursor=FALSE, ms.level=0) {
    ":\n\nGets a list of M/Z values contained inside the database.
    \nms.mode: The MS mode. Set it to either 'neg' or 'pos' to limit the output
    to one mode.
    \nmax.results: If set, it is used to limit the size of the output.
    \nprecursor: If set to TRUE, then restrict the search to precursor peaks.
    \nms.level: The MS level to which you want to restrict your search.
    0 means that you want to search in all levels.
    \nReturned value: A numeric vector containing M/Z values.
    "

    .self$.doGetMzValues(ms.mode=ms.mode, max.results=max.results,
                         precursor=precursor, ms.level=ms.level)
},

getNbPeaks=function(mode=NULL, ids=NULL) {
    ":\n\nGets the number of peaks contained in the database.
    \nmode: The MS mode. Set it to either 'neg' or 'pos' to limit the counting
    to one mode.
    \nids: A character vector of entry identifiers (i.e.: accession numbers).
    Used to restrict the set of entries on which to run the algorithm.
    \nReturned value: The number of peaks, as an integer.
    "

    .self$.abstractMethod()
},

filterEntriesOnRt=function(entry.ids, rt, rt.unit, rt.tol, rt.tol.exp,
                           chrom.col.ids, match.rt) {
    ":\n\nFilters a list of entries on retention time values.
    \nentry.ids: A character vector of entry IDs.
    \nrt: A vector of retention times to match. Used if input.df is not set.
    Unit is specified by rt.unit parameter.
    \nrt.unit: The unit for submitted retention times. Either 's' or 'min'.
    \nrt.tol: The plain tolerance (in seconds) for retention times:
    input.rt - rt.tol <= database.rt <= input.rt + rt.tol.
    \nrt.tol.exp: A special exponent tolerance for retention times:
    input.rt - input.rt ** rt.tol.exp <= database.rt <= input.rt + input.rt **
    rt.tol.exp. This exponent is applied on the RT value in seconds. If both
    rt.tol and rt.tol.exp are set, the inequality expression becomes: input.rt -
    rt.tol - input.rt ** rt.tol.exp <= database.rt <= input.rt + rt.tol +
    input.rt ** rt.tol.exp.
    \nchrom.col.ids: IDs of chromatographic columns on which to match the
    retention time.
    \nmatch.rt: If set to TRUE, filters on RT values, otherwise does not do any
    filtering.
    \nReturned value: A character vector containing entry IDs after filtering.
    "

    .self$.checkRtParam(rt=rt, rt.unit=rt.unit, rt.tol=rt.tol,
                        rt.tol.exp=rt.tol.exp, chrom.col.ids=chrom.col.ids,
                        match.rt=match.rt)

    if (match.rt) {

        # Get entries
        .self$message('debug', 'Getting entries from spectra IDs.')
        entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(),
                                                          entry.ids, drop=FALSE)

        # Filter on chromatographic columns
        if ( ! is.null(chrom.col.ids) && length(chrom.col.ids) > 0) {
            fct <- function(e) {
                e$getFieldValue('chrom.col.id') %in% chrom.col.ids
            }
            entries <- entries[vapply(entries, fct, FUN.VALUE=TRUE)]
            .self$debug(length(entries),
                        ' spectra remaining after chrom col filtering: ',
                        paste(vapply((if (length(entries) <= 10) entries
                                      else entries[seq_len(10)]),
                                     function(e) e$getFieldValue('accession'),
                                     FUN.VALUE=''), collapse=', '), '.')
        }

        # Filter out entries with no RT values or no RT unit
        fct <- function(e) {
            e$hasField('chrom.rt') || (e$hasField('chrom.rt.min')
                                       && e$hasField('chrom.rt.max'))
        }
        has.chrom.rt.values <- vapply(entries, fct, FUN.VALUE=TRUE)
        entries <- entries[has.chrom.rt.values]
        n <- sum( ! has.chrom.rt.values) > 0
        if (n > 0)
            .self$debug('Filtered out ', n, ' entries having no RT values.')
        fct <- function(e) e$hasField('chrom.rt.unit')
        no.chrom.rt.unit <- ! vapply(entries, fct, FUN.VALUE=TRUE)
        if (any(no.chrom.rt.unit))
            .self$caution('No RT unit specified in entries ',
                          paste(vapply(entries[no.chrom.rt.unit],
                                       function(e) e$getFieldValue('accession'),
                                       FUN.VALUE=''),
                                collapse=', '),
                          ', impossible to match retention times.')

        # Compute RT range for this input, in seconds
        rt.range <- .self$.computeRtRange(rt=rt, rt.unit=rt.unit, rt.tol=rt.tol,
                                          rt.tol.exp=rt.tol.exp)

        # Loop on all entries
        entry.ids <- character()
        for (e in entries) {

            # Get RT min and max for this column, in seconds
            col.rt.range <- .self$.computeChromColRtRange(e)

            # Test and possibly keep entry
            .self$debug('Testing if RT value ', rt, ' (', rt.unit,
                        ') is in range [', col.rt.range$min, ';',
                        col.rt.range$max, '] (s) of database entry ',
                        e$getFieldValue('accession'), '. Used range (after',
                        ' applying tolerances) for RT value is [', rt.range$min,
                        ', ', rt.range$max, '] (s).')
            if ((rt.range$max >= col.rt.range$min)
                && (rt.range$min <= col.rt.range$max))
                entry.ids <- c(entry.ids, e$getFieldValue('accession'))
        }

        .self$debug(length(entry.ids),
                    ' spectra remaining after retention time filtering:',
                    paste((if (length(entry.ids) <= 10) entry.ids
                           else entry.ids[seq_len(10)]), collapse=', '), '.')
    }

    return(entry.ids)
},

searchForMassSpectra=function(mz.min=NULL, mz.max=NULL, mz=NULL, mz.shift=0.0,
                         mz.tol=NA_real_, mz.tol.unit='plain', 
                         rt=NULL, rt.unit=NA_character_, rt.tol=NA_real_,
                         rt.tol.exp=NA_real_, chrom.col.ids=NULL,
                         precursor=FALSE,
                         min.rel.int=NA_real_, ms.mode=NA_character_,
                         max.results=NA_integer_, ms.level=0) {
    ":\n\nSearches for entries (i.e.: spectra) that contain a peak around the given
    M/Z value. Entries can also be filtered on RT values. You can input either a
    list of M/Z values through mz argument and set a tolerance with mz.tol
    argument, or two lists of minimum and maximum M/Z values through mz.min and
    mz.max arguments.
    \nmz: A vector of M/Z values.
    \nmz.shift: A shift applied on all M/Z values.
    \nmz.tol: The M/Z tolerance, whose unit is defined by mz.tol.unit.
    \nmz.tol.unit: The type of the M/Z tolerance. Set it to either to 'ppm' or
    'plain'.
    \nmz.min: A vector of minimum M/Z values.
    \nmz.max: A vector of maximum M/Z values. Its length must be the same as
    `mz.min`.
    \nrt: A vector of retention times to match. Used if input.df is not set.
    Unit is specified by rt.unit parameter.
    \nrt.unit: The unit for submitted retention times. Either 's' or 'min'.
    \nrt.tol: The plain tolerance (in seconds) for retention times:
    input.rt - rt.tol <= database.rt <= input.rt + rt.tol.
    \nrt.tol.exp: A special exponent tolerance for retention times:
    input.rt - input.rt ** rt.tol.exp <= database.rt <= input.rt + input.rt **
    rt.tol.exp. This exponent is applied on the RT value in seconds. If both
    rt.tol and rt.tol.exp are set, the inequality expression becomes: input.rt -
    rt.tol - input.rt ** rt.tol.exp <= database.rt <= input.rt + rt.tol +
    input.rt ** rt.tol.exp.
    \nchrom.col.ids: IDs of chromatographic columns on which to match the
    retention time.
    \nprecursor: If set to TRUE, then restrict the search to precursor peaks.
    \nmin.rel.int: The minimum relative intensity, in percentage (i.e.: float
    number between 0 and 100).
    \nms.mode: The MS mode. Set it to either 'neg' or 'pos'.
    \nms.level: The MS level to which you want to restrict your search.
    0 means that you want to search in all levels.
    \nmax.results: If set, it is used to limit the number of matches found for
    each M/Z value.
    \nReturned value: A character vector of spectra IDs.
    "

    # Check arguments
    check.param <- .self$.checkSearchMsParam(mz.min=mz.min, mz.max=mz.max,
        mz=mz, mz.shift=mz.shift, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit, rt=rt,
        rt.unit=rt.unit, rt.tol=rt.tol, rt.tol.exp=rt.tol.exp,
        chrom.col.ids=chrom.col.ids, min.rel.int=min.rel.int, ms.mode=ms.mode,
        max.results=max.results, ms.level=ms.level, match.rt=FALSE)
    if (is.null(check.param))
        return(NULL)

    ids <- character()

    if ((check.param$use.mz.min.max && ! all(is.na(mz.min) & is.na(mz.max)))
        || (check.param$use.mz.tol && ! all(is.na(mz)))) {

        if (check.param$use.rt.match) {
            # Search for one M/Z at a time
            for (i in seq_along(rt)) {

                # Search for this M/Z value
                if (check.param$use.mz.min.max)
                    mz.ids <- .self$.doSearchMzRange(mz.min=mz.min[[i]],
                        mz.max=mz.max[[i]], min.rel.int=min.rel.int,
                        ms.mode=ms.mode, max.results=NA_integer_,
                        precursor=precursor, ms.level=ms.level)
                else
                    mz.ids <- .self$.doSearchMzTol(mz=mz[[i]], mz.tol=mz.tol,
                        mz.tol.unit=mz.tol.unit, min.rel.int=min.rel.int,
                        ms.mode=ms.mode, max.results=NA_integer_,
                        precursor=precursor, ms.level=ms.level)

                # Filter on RT value
                rt.ids <- .self$filterEntriesOnRt(mz.ids, rt=rt[[i]],
                    rt.unit=rt.unit, rt.tol=rt.tol, rt.tol.exp=rt.tol.exp,
                    chrom.col.ids=chrom.col.ids,
                    match.rt=check.param$use.rt.match)

                ids <- c(ids, rt.ids)
            }
        }

        else {
            # Search for all M/Z values
            if (check.param$use.mz.min.max)
                ids <- .self$.doSearchMzRange(mz.min=mz.min, mz.max=mz.max,
                    min.rel.int=min.rel.int, ms.mode=ms.mode,
                    max.results=max.results, precursor=precursor,
                    ms.level=ms.level)
            else
                ids <- .self$.doSearchMzTol(mz=mz, mz.tol=mz.tol,
                    mz.tol.unit=mz.tol.unit, min.rel.int=min.rel.int,
                    ms.mode=ms.mode, max.results=max.results,
                    precursor=precursor, ms.level=ms.level)
        }
    }

    # Remove duplicates
    ids <- ids[ ! duplicated(ids)]

    # Cut
    if ( ! is.na(max.results) && length(ids) > max.results)
        ids <- ids[seq_len(max.results)]

    return(ids)
},

searchMsEntries=function(mz.min=NULL, mz.max=NULL, mz=NULL, mz.shift=0.0,
                         mz.tol=NA_real_, mz.tol.unit='plain', 
                         rt=NULL, rt.unit=NA_character_, rt.tol=NA_real_,
                         rt.tol.exp=NA_real_, chrom.col.ids=NULL,
                         precursor=FALSE,
                         min.rel.int=NA_real_, ms.mode=NA_character_,
                         max.results=NA_integer_, ms.level=0) { # DEPRECATED
    ":\n\nThis method is deprecated.
    \nUse searchForMassSpectra() instead.
    "
    .self$.deprecatedMethod("searchForMassSpectra()")
    return(.self$searchForMassSpectra(mz.min=mz.min, mz.max=mz.max, mz=mz,
                                      mz.shift=mz.shift, mz.tol=mz.tol,
                                      mz.tol.unit=mz.tol.unit, rt=rt,
                                      rt.unit=rt.unit,  rt.tol= rt.tol,
                                      rt.tol.exp=rt.tol.exp,
                                      chrom.col.ids=chrom.col.ids,
                                      precursor=precursor,
                                      min.rel.int=min.rel.int, ms.mode=ms.mode,
                                      ms.level=ms.level,
                                      max.results=max.results))
},

searchMsPeaks=function(input.df=NULL, mz=NULL, mz.shift=0.0, mz.tol,
    mz.tol.unit='plain', min.rel.int=NA_real_, ms.mode=NA_character_,
    ms.level=0, max.results=NA_integer_, chrom.col.ids=NULL, rt=NULL,
    rt.unit=NA_character_, rt.tol=NA_real_, rt.tol.exp=NA_real_,
    precursor=FALSE, precursor.rt.tol=NA_real_, insert.input.values=TRUE,
    prefix=NULL, compute=TRUE, fields=NULL, fieldsLimit=0,
    input.df.colnames=c(mz='mz', rt='rt'), match.rt=FALSE) {
    ":\n\nFor each M/Z value, searches for matching MS spectra and returns the
    matching peaks.
    \ninput.df: A data frame taken as input for searchMsPeaks(). It must
    contain a columns 'mz', and optionaly an 'rt' column.
    \nmz: A vector of M/Z values to match. Used if input.df is not set.
    \nmz.shift: A shift applied on all M/Z values.
    \nmz.tol: The M/Z tolerance, whose unit is defined by mz.tol.unit.
    \nmz.tol.unit: The type of the M/Z tolerance. Set it to either to 'ppm' or
    'plain'.
    \nmin.rel.int: The minimum relative intensity, in percentage (i.e.: float
    number between 0 and 100).
    \nms.mode: The MS mode. Set it to either 'neg' or 'pos'.
    \nms.level: The MS level to which you want to restrict your search.
    0 means that you want to search in all levels.
    \nmax.results: If set, it is used to limit the number of matches found for
    each M/Z value.
    \nchrom.col.ids: IDs of chromatographic columns on which to match the
    retention time.
    \nrt: A vector of retention times to match. Used if input.df is not set.
    Unit is specified by rt.unit parameter.
    \nrt.unit: The unit for submitted retention times. Either 's' or 'min'.
    \nrt.tol: The plain tolerance (in seconds) for retention times:
    input.rt - rt.tol <= database.rt <= input.rt + rt.tol.
    \nrt.tol.exp: A special exponent tolerance for retention times:
    input.rt - input.rt ** rt.tol.exp <= database.rt <= input.rt + input.rt **
    rt.tol.exp. This exponent is applied on the RT value in seconds. If both
    rt.tol and rt.tol.exp are set, the inequality expression becomes: input.rt -
    rt.tol - input.rt ** rt.tol.exp <= database.rt <= input.rt + rt.tol +
    input.rt ** rt.tol.exp.
    \nprecursor: If set to TRUE, then restrict the search to precursor peaks.
    \nprecursor.rt.tol: The RT tolerance used when matching the precursor.
    \ninsert.input.values: Insert input values at the beginning of the
    result data frame.
    \nprefix: Add prefix on column names of result data frame.
    \ncompute: If set to TRUE, use the computed values when converting found
    entries to data frame.
    \nfields: A character vector of field names to output. The data frame output
    will be restricted to this list of fields.
    \nfieldsLimit: The maximum of values to output for fields with multiple
    values. Set it to 0 to get all values.
    \ninput.df.colnames: Names of the columns in the input data frame.
    \nmatch.rt: If set to TRUE, match also RT values.
    \nReturned value: A data frame with at least input MZ and RT columns, and
    annotation columns prefixed with `prefix` if set. For each
    matching found a row is output. Thus if n matchings are found for M/Z value
    x, then there will be n rows for x, each for a different match. The number
    of matching found for each M/Z value is limited to `max.results`.
    "

    # Check arguments
    check.param <- .self$.checkSearchMsParam(input.df=input.df, mz.min=NULL,
        mz.max=NULL, mz=mz, mz.shift=mz.shift, mz.tol=mz.tol,
        mz.tol.unit=mz.tol.unit, rt=rt, rt.unit=rt.unit, rt.tol=rt.tol,
        rt.tol.exp=rt.tol.exp, chrom.col.ids=chrom.col.ids,
        min.rel.int=min.rel.int, ms.mode=ms.mode, max.results=max.results,
        ms.level=ms.level, input.df.colnames=input.df.colnames,
        match.rt=match.rt)
    if (is.null(check.param))
        return(NULL)
    input.df <- check.param$input.df

    results <- NULL
    result.columns <- character()

    # Step 1 matching of entries with matched precursor
    precursor.match.ids <- NULL
    if (precursor) {
        precursor.match.ids <- .self$searchForMassSpectra(mz.min=NULL, mz.max=NULL,
            mz=input.df[[input.df.colnames[['mz']]]], mz.shift=mz.shift,
            mz.tol=mz.tol, mz.tol.unit=mz.tol.unit,
            rt=input.df[[input.df.colnames[['rt']]]], rt.unit=rt.unit,
            rt.tol=precursor.rt.tol, chrom.col.ids=chrom.col.ids,
            precursor=precursor, min.rel.int=min.rel.int, ms.mode=ms.mode,
            ms.level=ms.level)
        .self$debug('Found ', length(precursor.match.ids),
                    ' spectra with matched precursor: ',
                    paste((if (length(precursor.match.ids) <= 10)
                           precursor.match.ids else
                               precursor.match.ids[seq_len(10)]),
                          collapse=', '), '.')
    }

    # Loop on the list of M/Z values
    .self$message('debug', 'Looping on all M/Z values.')
    .self$debug2List('M/Z values to process', input.df[[input.df.colnames[['mz']]]])
    for (i in seq_along(input.df[[input.df.colnames[['mz']]]])) {

        # Compute M/Z range
        mz <- input.df[i, input.df.colnames[['mz']]]
        mz.range <- .self$.convertMzTolToRange(mz=mz, mz.shift=mz.shift,
            mz.tol=mz.tol, mz.tol.unit=mz.tol.unit)

        # Search for spectra
        .self$debug('Searching for spectra that contains M/Z value in range [',
                    mz.range$min, ', ', mz.range$max, '].')
        ids <- .self$searchForMassSpectra(mz.min=mz.range$min, mz.max=mz.range$max,
            min.rel.int=min.rel.int, ms.mode=ms.mode,
            max.results=if (check.param$use.rt.match) NA_integer_
            else max.results, ms.level=ms.level)
        .self$debug2List('Found spectra', ids)

        # Filter out IDs that were not found in step 1.
        if ( ! is.null(precursor.match.ids)) {
            ids <- ids[ids %in% precursor.match.ids]
            .self$debug('After filtering on IDs with precursor match, we have ',
                        length(ids), ' spectra: ',
                        paste((if (length(ids) <= 10) ids
                               else ids[seq_len(10)]), collapse=', '), '.')
        }

        # Filter on RT value
        if  (check.param$use.rt.match) {
            rt <- input.df[i, input.df.colnames[['rt']]]
            ids <- .self$filterEntriesOnRt(ids, rt=rt, rt.unit=rt.unit,
                rt.tol=rt.tol, rt.tol.exp=rt.tol.exp,
                chrom.col.ids=chrom.col.ids, match.rt=check.param$use.rt.match)
        }

        # Get entries
        .self$debug('Getting entries from spectra IDs.')
        entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids,
                                                          drop=FALSE)

        # Cut
        if ( ! is.na(max.results) && length(entries) > max.results) {
            .self$debug('Cutting data frame', max.results, 'rows.')
            entries <- entries[seq_len(max.results)]
        }

        # Remove NULL entries
        null.entries <- vapply(entries, is.null, FUN.VALUE=TRUE)
        if (any(null.entries))
            .self$message('debug', 'One of the entries is NULL.')
        entries <- entries[ ! null.entries]

        # Display first entry
        if (length(entries) > 0)
                .self$debug('Field names of entry:',
                            paste(entries[[1]]$getFieldNames(), collapse=', '))

        # Convert to data frame
        .self$message('debug', 'Converting list of entries to data frame.')
        df <- .self$getBiodb()$entriesToDataframe(entries,
                                                  only.atomic=FALSE,
                                                  compute=compute,
                                                  flatten=FALSE,
                                                  limit=fieldsLimit)
        .self$debug2Dataframe('Entries obtained', df)

        # Select lines with right M/Z values
        mz <- input.df[i, input.df.colnames[['mz']]]
        mz.range <- .self$.convertMzTolToRange(mz=mz, mz.shift=mz.shift,
                                               mz.tol=mz.tol,
                                               mz.tol.unit=mz.tol.unit)
        .self$debug("Filtering entries data frame on M/Z range [", mz.range$min,
                    ', ', mz.range$max, '].')
        df <- df[(df$peak.mz >= mz.range$min) & (df$peak.mz <= mz.range$max), ]
        .self$debug2Dataframe('After filtering on M/Z range', df)

        # Select fields
        if ( ! is.null(fields))
            df <- df[fields[fields %in% colnames(df)]]

        # Add prefix on column names
        if ( ! is.null(df) && ncol(df) > 0 && ! is.null(prefix)
            && ! is.na(prefix))
            colnames(df) <- paste0(prefix, colnames(df))

        # Register result columns
        if ( ! is.null(df)) {
            newCols <- colnames(df)[ ! colnames(df) %in% result.columns]
            result.columns <- c(result.columns, newCols)
        }

        # Inserting input values at the beginning of the data frame
        if (insert.input.values) {
            df <- if (is.null(df) || nrow(df) == 0) input.df[i, , drop=FALSE]
                else cbind(input.df[i, , drop=FALSE], df, row.names=NULL,
                           stringsAsFactors=FALSE)
        }

        # Appending to main results data frame
        .self$debug('Merging data frame of matchings into results data frame.')
        results <- plyr::rbind.fill(results, df)
        .self$debug('Total results data frame contains', nrow(results), 'rows.')
    }

    # Sort result columns. We sort at the end of the processing, because result
    # data frames may contain different number of column, depending on the
    # presence of NA values.
    if ( ! is.null(results)) {
        isAnInputCol <- ! colnames(results) %in% result.columns
        inputCols <- colnames(results)[isAnInputCol]
        results <- results[, c(inputCols, sort(result.columns)), drop=FALSE]
    }

    return(results)
},

msmsSearch=function(spectrum, precursor.mz, mz.tol, mz.tol.unit='plain',
    ms.mode, npmin=2,
    dist.fun=c('wcosine', 'cosine', 'pkernel', 'pbachtttarya'), msms.mz.tol=3,
    msms.mz.tol.min=0.005, max.results=NA_integer_) {
    ":\n\nSearches MSMS spectra matching a template spectrum. The mz.tol
    parameter is applied on the precursor search.
    \nspectrum: A template spectrum to match inside the database.
    \nprecursor.mz: The M/Z value of the precursor peak of the mass spectrum.
    \nmz.tol: The M/Z tolerance, whose unit is defined by mz.tol.unit.
    \nmz.tol.unit: The type of the M/Z tolerance. Set it to either to 'ppm' or
    'plain'.
    \nms.mode: The MS mode. Set it to either 'neg' or 'pos'.
    \nnpmin: The minimum number of peak to detect a match (2 is recommended).
    \ndist.fun: The distance function used to compute the distance betweem two
    mass spectra.
    \nmsms.mz.tol: M/Z tolerance to apply while matching MSMS spectra.  In PPM.
    \nmsms.mz.tol.min: Minimum of the M/Z tolerance (plain unit). If the M/Z
    tolerance computed with `msms.mz.tol` is lower than `msms.mz.tol.min`, then
    `msms.mz.tol.min` will be used.
    \nmax.results: If set, it is used to limit the number of matches found for
    each M/Z value.
    \nReturned value: A data frame with columns `id`, `score` and `peak.*`. Each
    `peak.*` column corresponds to a peak in the input spectrum, in the same
    order and gives the number of the peak that was matched with it inside the
    matched spectrum whose ID is inside the `id` column.
    "

    peak.tables <- list()
    dist.fun <- match.arg(dist.fun)

    # Get spectra IDs
    ids <- character()
    if ( ! is.null(spectrum) && nrow(spectrum) > 0 && ! is.null(precursor.mz)) {
        if ( ! is.na(max.results))
            .self$caution('Applying max.results =', max.results,'on call to',
                ' searchForMassSpectra(). This may results in no matches, while there',
                ' exist matching spectra inside the database.')
        ids <- .self$searchForMassSpectra(mz=precursor.mz, mz.tol=mz.tol,
            mz.tol.unit=mz.tol.unit, ms.mode=ms.mode, precursor=TRUE,
            ms.level=2, max.results=max.results)
    }

    # Get list of peak tables from spectra
    if (length(ids) > 0) {
        entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids,
                                                          drop=FALSE)
        fct <- function(x) {
            x$getFieldsAsDataframe(only.atomic=FALSE, flatten=FALSE,
                                   fields=c('peak.mz',
                                            'peak.relative.intensity',
                                            'peak.intensity'))
        }
        peak.tables <- lapply(entries, fct)
    }

    # Compare spectrum against database spectra
    res <- compareSpectra(spectrum, peak.tables, npmin=npmin, fun=dist.fun,
                          params=list(ppm=msms.mz.tol, dmz=msms.mz.tol.min))
    
    cols <- colnames(res)
    res[['id']] <- ids
    res <- res[, c('id', cols)]
    
    # Order rows
    res <- res[order(res[['score']], decreasing=TRUE), ]
    
    return(res)
},

collapseResultsDataFrame=function(results.df, mz.col='mz', rt.col='rt',
                                  sep='|') {
    ":\n\nCollapse rows of a results data frame, by outputing a data frame with only
    one row for each MZ/RT value.
    \nresults.df: Results data frame.
    \n mz.col: The name of the M/Z column in the results data frame.
    \n rt.col: The name of the RT column in the results data frame.
    \n sep:    The separator used to concatenate values, when
               collapsing results data frame.
    \nReturned value: A data frame with rows collapsed."

    cols <- mz.col
    if (rt.col %in% colnames(results.df))
        cols <- c(cols, rt.col)
    x <- .self$getBiodb()$collapseRows(results.df, cols=cols)
    
    return(x)
},

searchMzRange=function(mz.min, mz.max, min.rel.int=NA_real_,
                       ms.mode=NA_character_, max.results=NA_integer_,
                       precursor=FALSE, ms.level=0) {
    "Find spectra in the given M/Z range. Returns a list of spectra IDs."

    .self$.deprecatedMethod('BiodbMassdbConn::searchForMassSpectra()')

    return(.self$searchForMassSpectra(mz.min=mz.min, mz.max=mz.max,
        min.rel.int=min.rel.int, ms.mode=ms.mode, max.results=max.results,
        precursor=precursor, ms.level=ms.level))
},

searchMzTol=function(mz, mz.tol, mz.tol.unit='plain', min.rel.int=NA_real_,
                     ms.mode=NA_character_, max.results=NA_integer_,
                     precursor=FALSE, ms.level=0) {
    "Find spectra containg a peak around the given M/Z value. Returns a
    character vector of spectra IDs."

    .self$.deprecatedMethod('BiodbMassdbConn::searchForMassSpectra()')
    
    return(.self$searchForMassSpectra(mz=mz, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit,
        min.rel.int=min.rel.int, ms.mode=ms.mode, max.results=max.results,
        precursor=precursor, ms.level=ms.level))
},

.convertMzTolToRange=function(mz, mz.shift, mz.tol, mz.tol.unit) {

    if (mz.tol.unit == 'ppm') {
        mz.min <- mz + mz * ( mz.shift - mz.tol) * 1e-6
        mz.max <- mz + mz * ( mz.shift + mz.tol) * 1e-6
    }
    else {
        mz.min <- mz + mz.shift - mz.tol
        mz.max <- mz + mz.shift + mz.tol
    }


    return(list(min=mz.min, max=mz.max))
},

.doSearchMzTol=function(mz, mz.tol, mz.tol.unit, min.rel.int, ms.mode,
                        max.results, precursor, ms.level) {

    range <- .self$.convertMzTolToRange(mz=mz, mz.shift=0.0, mz.tol=mz.tol,
                                        mz.tol.unit=mz.tol.unit)

    return(.self$searchForMassSpectra(mz.min=range$min, mz.max=range$max,
        min.rel.int=min.rel.int, ms.mode=ms.mode, max.results=max.results,
        precursor=precursor, ms.level=ms.level))
},

.doSearchMzRange=function(mz.min, mz.max, min.rel.int, ms.mode, max.results,
                          precursor, ms.level) {
    .self$.abstractMethod()
},

.doGetMzValues=function(ms.mode, max.results, precursor, ms.level) {
    .self$.abstractMethod()
},

.convertRt=function(rt, units, wanted.unit) {

    # RT values with wrong unit
    rt.wrong <- units != wanted.unit

    # Convert any RT value using wrong unit
    if (any(rt.wrong)) {
        if ('s' %in% units[rt.wrong]) {
            if (wanted.unit != 'min')
                .self$error('Error when converting retention times values.',
                            ' Was expecting "min" for target unit.')
            rt[rt.wrong] <- rt[rt.wrong] / 60
        }
        if ('min' %in% units[rt.wrong]) {
            if (wanted.unit != 's')
                .self$error('Error when converting retention times values.',
                            ' Was expecting "s" for target unit.')
            rt[rt.wrong] <- rt[rt.wrong] * 60
        }
    }

    return(rt)
},

.checkMzMinMaxParam=function(mz.min, mz.max) {

    use.min.max <- ! is.null(mz.min) && ! is.null(mz.max)

    if (use.min.max) {
        .self$.assertIs(mz.min, c('numeric', 'integer'))
        .self$.assertIs(mz.max, c('numeric', 'integer'))
        .self$.assertPositive(mz.min)
        .self$.assertPositive(mz.max)
        .self$.assertEqualLength(mz.min, mz.max)
        .self$.assertInferior(mz.min, mz.max)
    }

    return(use.min.max)
},

.checkMzTolParam=function(mz, mz.shift, mz.tol, mz.tol.unit) {

    use.tol <- ! is.null(mz)

    if (use.tol) {
        .self$.assertIs(mz, c('numeric', 'integer'))
        .self$.assertPositive(mz)
        .self$.assertPositive(mz.tol)
        .self$.assertLengthOne(mz.tol)
        .self$.assertIn(mz.tol.unit, c('ppm', 'plain'))
    }

    return(use.tol)
},

.checkMzParam=function(mz.min, mz.max, mz, mz.shift, mz.tol, mz.tol.unit) {

    use.tol <- .self$.checkMzTolParam(mz=mz, mz.shift=mz.shift, mz.tol=mz.tol,
                                      mz.tol.unit=mz.tol.unit)
    use.min.max <- .self$.checkMzMinMaxParam(mz.min=mz.min, mz.max=mz.max)

    if (use.tol && use.min.max)
        .self$error("You cannot set both mz and (mz.min, mz.max). Please",
            " choose one of those these two schemes to input M/Z values.")

    return(list(use.tol=use.tol, use.min.max=use.min.max))
},

.checkRtParam=function(rt, rt.unit, rt.tol, rt.tol.exp, chrom.col.ids,
                       match.rt) {

    if (match.rt) {
        .self$.assertIs(rt, c('numeric', 'integer'))
        .self$.assertPositive(rt)
        .self$.assertPositive(rt.tol)
        .self$.assertPositive(rt.tol.exp)
        if ( ! is.null(chrom.col.ids)) {
            .self$.assertNotNa(chrom.col.ids)
            .self$.assertIs(chrom.col.ids, 'character')
        }
        .self$.assertNotNa(rt.unit)
        .self$.assertIn(rt.unit, c('s', 'min'))
        .self$.assertLengthOne(rt.unit)
    }
},

.checkSearchMsParam=function(input.df=NULL, input.df.colnames=c(mz='mz',
    rt='rt', mz.min='mz.min', mz.max='mz.max'), mz.min, mz.max, mz, mz.shift,
    mz.tol, mz.tol.unit, rt, rt.unit, rt.tol, rt.tol.exp, chrom.col.ids,
    min.rel.int, ms.mode, max.results, ms.level, match.rt) {

    match.rt <- match.rt || ! is.null(rt)

    # Set M/Z and RT input values
    if ( ! is.null(input.df)) {
        if (is.vector(input.df)) {
            input.df <- data.frame(mz=input.df)
            if ( ! 'mz' %in% colnames(input.df.colnames))
                input.df.colnames[['mz']] <- 'mz'
            colnames(input.df) <- input.df.colnames[['mz']]
        }
        .self$.assertIs(input.df, 'data.frame')
        for (v in c('mz', 'mz.min', 'mz.max', 'rt')) {
            if (is.null(get(v)) && v %in% names(input.df.colnames)
                && ! is.null(input.df.colnames[[v]])
                && ! is.na(input.df.colnames[[v]])
                && input.df.colnames[[v]] %in% colnames(input.df))
                assign(v, input.df[[input.df.colnames[[v]]]])
        }
    }

    mz.match <- .self$.checkMzParam(mz.min=mz.min, mz.max=mz.max, mz=mz,
        mz.shift=mz.shift, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit)
    .self$.checkRtParam(rt=rt, rt.unit=rt.unit, rt.tol=rt.tol,
        rt.tol.exp=rt.tol.exp, chrom.col.ids=chrom.col.ids, match.rt=match.rt)
    if ( ! mz.match$use.tol && ! mz.match$use.min.max)
        return(NULL)
    if (mz.match$use.tol && match.rt && length(mz) != length(rt))
        .self$error('mz and rt must have the same length.')
    if (mz.match$use.min.max && match.rt && length(mz.min) != length(rt))
        .self$error('mz.min, mz.max and rt must have the same length.')

    # Set input data frame
    for (v in c('mz', 'mz.min', 'mz.max', 'rt')) {
        if ( ! is.null(get(v))) {
            if (is.null(input.df)) {
                input.df <- data.frame(x=get(v))
                colnames(input.df) <- v
            } else {
                if (nrow(input.df) != length(get(v)))
                    .self$error('input.df (length ', nrow(input.df), '), and ',
                        v, ' (length ', length(get(v)),
                        ') must have the same length.')
                else {
                    if ( ! v %in% names(input.df.colnames))
                        input.df.colnames[[v]] <- v
                    input.df[[input.df.colnames[[v]]]] <- get(v)
                }
            }
        }
    }

    .self$.assertPositive(min.rel.int)
    ef <- .self$getBiodb()$getEntryFields()
    .self$.assertIn(ms.mode, ef$get('ms.mode')$getAllowedValues())
    ms.mode <- ef$get('ms.mode')$correctValue(ms.mode)
    .self$.assertPositive(max.results)
    .self$.assertPositive(ms.level)

    return(list(use.mz.tol=mz.match$use.tol,
                use.mz.min.max=mz.match$use.min.max, use.rt.match=match.rt,
                input.df=input.df))
},

.computeChromColRtRange=function(entry) {

    rt.col.unit <- entry$getFieldValue('chrom.rt.unit')

    if (entry$hasField('chrom.rt')) {
        rt.col.min <- .self$.convertRt(entry$getFieldValue('chrom.rt'),
                                       rt.col.unit, 's')
        rt.col.max <- rt.col.min
    } else if (entry$hasField('chrom.rt.min')
               && entry$hasField('chrom.rt.max')) {
        rt.col.min <- .self$.convertRt(entry$getFieldValue('chrom.rt.min'),
                                       rt.col.unit, 's')
        rt.col.max <- .self$.convertRt(entry$getFieldValue('chrom.rt.max'),
                                       rt.col.unit, 's')
    } else
        .self$error('Impossible to match on retention time, no retention time',
            ' fields (chrom.rt or chrom.rt.min and chrom.rt.max) were found.')

    return(list(min=rt.col.min, max=rt.col.max))
},

.computeRtRange=function(rt, rt.unit, rt.tol, rt.tol.exp) {

    rt.sec <- .self$.convertRt(rt, rt.unit, 's')
    rt.min <- rt.sec
    rt.max <- rt.sec
    .self$debug('At step 1, RT range is [', rt.min, ', ', rt.max, '] (s).')
    if ( ! is.na(rt.tol)) {
        .self$message('debug', paste0('RT tol is ', rt.tol, ' (s).'))
        rt.min <- rt.min - rt.tol
        rt.max <- rt.max + rt.tol
    }
    .self$debug('At step 2, RT range is [', rt.min, ', ', rt.max, '] (s).')
    if ( ! is.na(rt.tol.exp)) {
        .self$message('debug', paste0('RT tol exp is ', rt.tol.exp, '.'))
        rt.min <- rt.min - rt.sec ** rt.tol.exp
        rt.max <- rt.max + rt.sec ** rt.tol.exp
    }
    .self$debug('At step 3, RT range is [', rt.min, ', ', rt.max, '] (s).')

    return(list(min=rt.min, max=rt.max))
}

))
