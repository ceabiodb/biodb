# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbObject {{{1
################################################################################

# Declaration {{{2
################################################################################

BiodbObject <- methods::setRefClass("BiodbObject",
    fields=list(
                .message.enabled="logical"
                ),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function() {

    .self$.abstractClass('BiodbObject')

    .self$.message.enabled <- TRUE
},

# Get biodb {{{3
################################################################################

getBiodb=function() {
    .self$abstract.method()
},

# Notify observers {{{3
################################################################################

notify=function(fct, args) {
    
    # Get observers
    obs <- .self$getBiodb()$getObservers()
    
    # Build call code
    call <- paste0('do.call(o$', fct, ', args)')

    # Notify each observer
    lapply(obs, function(o) eval(parse(text=call)) )
},

# Progress message {{{3
################################################################################

progressMsg=function(msg, index, first, total=NA_integer_, type='info') {
    .self$notify('progress', list(type=type, msg=msg, index=index, total=total,
                                  first=first))
},

# Message {{{3
################################################################################

# Send a message to observers
message=function(type, msg, lvl=1) {

    type <- tolower(type)
    
    # Get biodb instance
    biodb <- NULL
    if (length(.self$.message.enabled) == 1 # length is 0
                                            # if called from constructor
        && .self$.message.enabled) {
        .self$.message.enabled <- FALSE
        biodb <- .self$getBiodb() 
        .self$.message.enabled <- TRUE
    }

    # Get class and method information
    class <- class(.self)
    method <- sys.call(length(sys.calls()) - 1)
    method <- sub('^[^$]*\\$([^(]*)(\\(.*)?$', '\\1()', method)[[1]]

    if ( ! is.null(biodb))
        .self$notify('msg', list(type=type, msg=msg, class=class, method=method,
                                 lvl=lvl))
    else {
        caller.info <- if (is.na(class)) '' else class
        if (! is.na(method))
            caller.info <- paste(caller.info, method, sep='::')
        if (nchar(caller.info) > 0)
            caller.info <- paste('[', caller.info, '] ', sep='')
        switch(type,
               error=stop(caller.info, msg),
               warning=warning(caller.info, msg),
               base::message(caller.info, msg))
    }
},

# Debug message {{{3
################################################################################

debug=function(...) {
    .self$message(type='debug', msg=paste0(...))
},

# Debug message level 2 {{{3
################################################################################

debug2=function(...) {
    .self$message(type='debug', msg=paste0(...), lvl=2)
},

# Debug message level 2 for printing list {{{3
################################################################################

debug2List=function(msg, lst, cut=10) {
    
    if (length(lst) == 0)
        s <- 'none'
    else {
        s <- paste(if (length(lst) > 10) c(lst[seq_len(10)], '...') else lst,
                   collapse=", ")
        s <- paste0('"', s, '"')
    }
    .self$debug2(msg, '[', length(lst), ']: ', s, '.')
},

# Error message {{{3
################################################################################

error=function(...) {
    .self$message(type='error', msg=paste0(...))
},

# Warning message {{{3
################################################################################

warning=function(...) {
    .self$message(type='warning', msg=paste0(...))
},

# Caution message {{{3
################################################################################

caution=function(...) {
    .self$message(type='caution', msg=paste0(...))
},

# Info message {{{3
################################################################################

info=function(...) {
    .self$message(type='info', msg=paste0(...))
},

# Info message level 2 {{{3
################################################################################

info2=function(...) {
    .self$message(type='info', msg=paste0(...), lvl=2)
},

# Private methods {{{2
################################################################################

# Abstract class {{{3R
################################################################################

# This method is used to declare a class as abstract.
.abstractClass=function(class) {

    if (class == class(.self))
        .self$error('Class ', class, ' is abstract and thus cannot be ',
                    'instantiated.')
},

# Abstract method {{{3
################################################################################

# This method is used to declare a method as abstract.
.abstractMethod=function() {

    class <- class(.self)
    method <- sys.calls()[[length(sys.calls()) - 1]]
    method <- as.character(method)
    method <- method[[1]]
    method <- sub('^[^$]*\\$([^(]*)(\\(.*)?$', '\\1()', method)

    .self$error("Method ", method, " is not implemented in ", class, " class.")
},

# Deprecated method {{{3
################################################################################

# This method is used to declare a method as deprecated.
.deprecatedMethod=function(new.method=NA_character_) {

    class <- class(.self)
    call <- sys.call(-1)
    call <- as.character(call)
    call <- call[[1]]
    calls <- strsplit(call, '$', fixed=TRUE)[[1]]
    method <- calls[[length(calls)]]

    msg <- paste("Method ", method, "() is now deprecated in ",
                 class, " class.", sep='')
    if ( ! is.na(new.method))
        msg <- paste(msg, " Please use now method ", new.method, ".", sep='')
    .self$message('caution', msg)
},

# Assert not NA {{{3
################################################################################

.assertNotNa=function(param, msg.type='error', sys.call.level=0,
                      param.name=NULL) {

    if (any(is.na(param))) {

        if (is.null(param.name))
            param.name <- as.character(sys.call(sys.call.level))[[2]]

        .self$message(msg.type, paste0(param.name, ' cannot be set to NA.'))

        return(FALSE)
    }

    return(TRUE)
},

# Assert not NULL {{{3
################################################################################

.assertNotNull=function(param, msg.type='error', sys.call.level=0,
                            param.name=NULL) {

    if (is.null(param)) {

        if (is.null(param.name))
            param.name <- as.character(sys.call(sys.call.level))[[2]]

        .self$message(msg.type, paste(param.name, ' cannot be NULL.', sep=''))

        return(FALSE)
    }
    return(TRUE)
},

# Assert inferior {{{3
################################################################################

.assertInferior=function(param1, param2, msg.type='error',
                            na.allowed=TRUE) {

    # Remove NA values
    if (na.allowed) {
        nas <- is.na(param1) | is.na(param2)
        param1 <- param1[ ! nas]
        param2 <- param2[ ! nas]
    }

    # Compare
    if (any(param1 > param2)) {
        param1.name <- as.character(sys.call(0))[[2]]
        param2.name <- as.character(sys.call(0))[[3]]
        .self$message(msg.type, paste0(param1.name, ' (', param1,
                                       ') cannot be greater than ', param2.name,
                                       ' (', param2, ').'))
        return(FALSE)
    }

    return(TRUE)
},

# Assert equal length {{{3
################################################################################

.assertEqualLength=function(param1, param2, msg.type='error') {
    if (length(param1) != length(param2)) {
        param1.name <- as.character(sys.call(0))[[2]]
        param2.name <- as.character(sys.call(0))[[3]]
        .self$message(msg.type, paste0(param1.name, ' (length ', length(param1),
                                      ') has not the same length as ',
                                      param2.name, ' (length ', length(param2),
                                      ').'))
        return(FALSE)
    }
    return(TRUE)
},

# Assert positive {{{3
################################################################################

.assertPositive=function(param, na.allowed=TRUE, zero=TRUE,
                            sys.call.level=0, msg.type='error',
                            param.name=NULL) {

    if (is.null(param.name))
        param.name <- as.character(sys.call(sys.call.level))[[2]]

    .self$.assertNotNull(param, msg.type=msg.type, param.name=param.name)
    if ( ! na.allowed)
        .self$.assertNotNa(param, msg.type=msg.type, param.name=param.name)

    if (any(param[ ! is.na(param)] < 0)
        || ( ! zero && any(param[ ! is.na(param)] == 0))) {
        .self$message(msg.type, paste0(param.name, ' (',
                                       paste(param, collapse=", "),
                                       ') cannot be negative',
                                       if (zero) '' else ' or equal to zero',
                                       '.'))
        return(FALSE)
    }

    return(TRUE)
},

# Assert length one {{{3
################################################################################

.assertLengthOne=function(param, sys.call.level=0, msg.type='error',
                              param.name=NULL) {

    if (length(param) != 1) {

        if (is.null(param.name))
            param.name <- as.character(sys.call(sys.call.level))[[2]]

        .self$message(msg.type, paste0('Length of ', param.name,
                                      ' (', length(param), ') must be one.'))

        return(FALSE)
    }
    return(TRUE)
},

# Assert in {{{3
################################################################################

.assertIn=function(param, values, msg.type='error') {
    if ( ! is.na(param) && ! param %in% values) {
        param.name <- as.character(sys.call(0))[[2]]
        .self$message(msg.type, paste0(param.name, ' cannot be set to ', param,
                                      '. Allowed values are: ',
                                      paste(values, collapse=', '), '.'))
        return(FALSE)
    }
    return(TRUE)
},

# Assert is {{{3
################################################################################

.assertIs=function(param, type, sys.call.level=0, msg.type='error',
                      param.name=NULL) {

    if ( ! is.null(param) && ! class(param) %in% type) {

        if (is.null(param.name))
            param.name <- as.character(sys.call(sys.call.level))[[2]]

        .self$message(msg.type, paste0(param.name, ' is not of type ', type,
                                      ' but of type ', class(param), '.'))

        return(FALSE)
    }

    return(TRUE)
},

# Assert inherits from {{{3
################################################################################

.assertInheritsFrom=function(param, super.class, msg.type='error') {
    if ( ! is.null(param) && ! is(param, super.class)) {
        param.name <- as.character(sys.call(0))[[2]]
        .self$message(msg.type, paste0(param.name, ' does not inherit from ',
                                       super.class, '.'))
        return(FALSE)
    }
    return(TRUE)
},

# Assert number {{{3
################################################################################

.assertNumber=function(param, length.one=TRUE, null.allowed=FALSE,
                          na.allowed=FALSE, zero=TRUE, negative=TRUE,
                          sys.call.level=0, integer.allowed=TRUE,
                          float.allowed=TRUE) {

    param.name <- as.character(sys.call(sys.call.level))[[2]]

    if ( ! null.allowed)
        .self$.assertNotNull(param, param.name=param.name)

    if ( ! is.null(param)) {

        if ( ! na.allowed)
            .self$.assertNotNa(param, param.name=param.name)

        types <- character()
        if (integer.allowed)
            types <- c(types, 'integer')
        if (float.allowed)
            types <- c(types, 'numeric')
        .self$.assertIs(param, types, param.name=param.name)

        if (length.one)
            .self$.assertLengthOne(param, param.name=param.name)

        if ( ! negative)
            .self$.assertPositive(param, na.allowed=na.allowed, zero=zero,
                                   param.name=param.name)
    }
}

))
